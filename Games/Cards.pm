#!/usr/bin/perl -w
#
# Games::Cards - Package for card games
#
# Copyright 1999 Amir Karger (karger@post.harvard.edu)
#
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

#TODO make Undo_List a my(), so that only this module can access it. See
# perltoot. (Need DESTROY method for Undos?)
# TODO use AUTOLOAD (pertoot) instead of all the object methods that just return
# the class. (Possibly use Lynn Wilson's method to allow *setting* too)
#
# TODO maybe someday: ExtraCards. In addition to Suits * Cards_In_Suit, have
# this field of cards added to the deck. E.g., jokers. (Or a "color card" for
# a dealer's card shoe.) These probably need values too.

package Games::Cards;

use strict;
use vars qw($VERSION);
require 5.004; # I use 'foreach my'

# Stolen from `man perlmod`
$VERSION = do { my @r = (q$Revision: 1.20 $ =~ /\d+/g); sprintf "%d."."%02d" x $#r, @r }; # must be all one line, for MakeMaker

# sub-packages
{ 
package Games::Cards::Game;
package Games::Cards::Queue;
package Games::Cards::Stack;
package Games::Cards::CardSet;
package Games::Cards::Card;
package Games::Cards::Undo;
package Games::Cards::Move;
}

=pod

=head1 NAME

Games::Cards -- Perl module for playing card games

=head1 SYNOPSIS

    use Games::Cards;

    my $Rummy = new Games::Cards::Game;

    # Create and shuffle the deck
    my $Deck = $Rummy->create_deck;
    $Deck->shuffle;

    # Deal out the hands
    foreach my $i (1 .. 3) {
	my $hand = new Games::Cards::Stack "Player $i" ;
	$Deck->give_cards($hand, 7);
	push @Hands, $hand;
    }

    # print hands (e.g. "Player 1: KD QD JD 1S 2C 3C 3H")
    foreach (@Hands) { print $_->print("short") }

    # Initialize the undo engine to save 100 moves
    Games::Cards::Undo->initialize(100);
    Games::Cards::Undo->undo; # undo last move
    Games::Cards::Undo->un_undo; # redo last undone move

=head1 DESCRIPTION

This module creates objects to allow easier programming of card games.

=head2 Class Games::Cards::Game

This class represents a certain game, like War, or Solitaire. This is
necessary to store the various rules for a given game, like the ranking
of the cards. (Or, for more exotic games, how many cards of what type are
in the deck.) Methods:

=over 4

=cut

{
package Games::Cards::Game;
# suits is a reference to an array listing the suits in the deck
# cards_in_suit is a reference to a hash whose keys are the names of the
#     cards in each suit, and values are the (default) values of those cards
# (Card names will be strings, although they might be "2". Values are
# integers, so that we can compare cards with other cards.)

my $Default_Suits = [qw(Clubs Diamonds Hearts Spades)];
# (Parts of) this hash will need to be reset in lots of games.
my $Default_Cards_In_Suit = {
    "Ace" => 1,
    2 => 2,
    3 => 3,
    4 => 4,
    5 => 5,
    6 => 6,
    7 => 7,
    8 => 8,
    9 => 9,
    10 => 10,
    "Jack" => 11,
    "Queen" => 12,
    "King" => 13,
};

=item new(HASHREF)

creates a new game. HASHREF is a reference to a hash containing zero or more
of the keys "suits" and "cards_in_suit". "suits" is a list of the suits in a
deck, "cards_in_suit" is a reference to a hash whose keys are the names
of the cards in one suit and whose values are the values (or ranks) of those
cards. If "suits" is not given, the default suits (Clubs, Diamonds, Hearts,
Spades) are used. If "cards_in_suit" is not given, the default cards
(Ace, 2..10, Jack, Queen, King with values 1..13) are used.
For example, war would require "Ace"=>14.

=cut

    sub new {
	my $class = shift;
	my $hashref = shift;
	my $cardgame = {
	    "suits" => $hashref->{"suits"} || $Default_Suits,
	    "cards_in_suit" => $hashref->{"cards_in_suit"} ||
	                       $Default_Cards_In_Suit,
	};

	bless $cardgame, $class;
    } # end sub Games::Cards::Game::new

=item create_deck

creates an I<unshuffled> deck of cards. For each card in the deck it creates
a name, suit, and value. The deck was stipulated by the 'new Games::Cards'
command. Returns the deck.

=cut

    sub create_deck {
	my $game = shift;
	my $deck = new Games::Cards::Queue "Deck";
	my %cards = %{$game->{"cards_in_suit"}};

	# make an unshuffled deck
	foreach my $suit (@{$game->{"suits"}}) {
	    foreach my $name (keys %cards) {
		my $new_card = new Games::Cards::Card {"suit"=>$suit, 
						      "name"=> $name,
						      "value" => $cards{$name}};
		push @{$deck->{"cards"}}, $new_card;
	    }
	}

	return $deck;
    } # end sub Games::Cards::Game::create_deck

} # end package Games::Cards::Game

=head2 Class Games::Cards::Queue

A queue (cf. computer science terminology, or the C++ stdlib) is a first-in
first-out pile of cards. Cards are removed from the top of the pile, but new
cards are added to the bottom of the pile.  This might represent, say, a pile
of face-down cards, like the player's hand in War.

=cut

{
package Games::Cards::Queue;
    @Games::Cards::Queue::ISA = ("Games::Cards::CardSet");

    sub new {
	my $class = shift;
	my $name = shift;
	my $thing = new Games::Cards::CardSet ($name);
	$thing->{"FIFO"} = 1;

	bless $thing, $class;
    } # end Games::Cards::Queue::new

    sub remove_cards {
    # remove (and return a ref to) top arg1 cards from the Queue
	my ($thing, $number) = @_;
	return [splice (@{$thing->{"cards"}}, -$number)];
    } # end sub Games::Cards::Queue::remove_cards

    sub add_cards {
    # Add array of Cards arg1 to the Queue
	my ($queue, $cards) = @_;
	unshift @{$queue->{"cards"}}, @$cards;
    } # end sub Games::Cards::Queue::add_cards

    sub un_remove_cards {
    # opposite of remove_cards (for Undo)
	my ($queue, $cards) = @_;
	push @{$queue->{"cards"}}, @$cards;
    } # end sub Games::Cards::Queue::un_remove_cards

    sub un_add_cards {
    # opposite of add_cards (for Undo)
	my ($thing, $number) = @_;
	return [splice (@{$thing->{"cards"}}, 0, $number)];
    } # end sub Games::Cards::Queue::un_add_cards

} #end package Games::Cards::Queue

=head2 Class Games::Cards::Stack

A stack (cf. computer science terminology, or the C++ stdlib) is a last-in
first-out pile of cards. Cards are removed from the top of the pile, and new
cards are also added to the top of the pile. This would usually represent a
pile of cards with its top card (and perhaps all cards) face up.

=cut

{
package Games::Cards::Stack;
    @Games::Cards::Stack::ISA = ("Games::Cards::CardSet");

    sub new {
	my $class = shift;
	my $name = shift;
	my $thing = new Games::Cards::CardSet $name;
	$thing->{"FIFO"} = 0;

	bless $thing, $class;
    } # end Games::Cards::Stack::new

    sub remove_cards {
    # remove (and return a ref to) top arg1 cards from the Stack
	my ($thing, $number) = @_;
	return [splice (@{$thing->{"cards"}}, -$number)];
    } # end sub Games::Cards::Stack::remove_cards

    sub add_cards {
    # Add array of Cards arg1 to the Stack
	my ($stack, $cards) = @_;
	push @{$stack->{"cards"}}, @$cards;
    } # end sub Games::Cards::Stack::add_cards

    sub un_remove_cards {
    # opposite of remove_cards (for Undo)
	my ($stack, $cards) = @_;
	push @{$stack->{"cards"}}, @$cards;
    } # end sub Games::Cards::Stack::un_remove_cards

    sub un_add_cards {
    # opposite of add_cards (for Undo)
	my ($thing, $number) = @_;
	return [splice (@{$thing->{"cards"}}, -$number)];
    } # end sub Games::Cards::Stack::un_add_cards

} #end package Games::Cards::Stack

######################################################################

=head2 Class Games::Cards::CardSet

A CardSet is just an array of cards (stored in the "cards" field). It could be
a player's hand, a deck, or a discard pile, for instance. This is a super class
of Queue and Stack, and those classes should be used instead, so that we know
whether the cards in the pile are FIFO or LIFO. Methods:

=over 4

=cut

{
package Games::Cards::CardSet;
# Fields:
# cards - array of Cards name - "Joe's Hand" for Joe's hand, "discard" for a
# discard pile, etc.
# The cards array is LIFO for the Stack subclass and FIFO for the Queue
# subclass. The "top" card in the Queue/Stack---the next card we would see if
# we played a card from that pile---is always cards[-1], so we always use
# "pop", but when adding cards, we use "push" for the Stack, and "unshift" for
# the Queue. 

    sub new {
    # arg0 is the class
        my $class = shift;
	my $name = shift;
	my $set = {
	    "cards" => [],
	    "name" => $name,
	};

	bless $set, $class;
    } # end sub CardSet::new

=item shuffle

shuffles the cards in the CardSet

=cut

    sub shuffle {
    # shuffle the deck
        my $deck = shift;

	# Schwarztian transform
	# Replace the cards in the deck with shuffled cards
	# (Just pick N random numbers & sort them)
	@{$deck->{"cards"}} =
	    map { $_->[0] } 
	    sort { $a->[1] <=> $b->[1] } 
	    map { [$_, rand] } 
	    @{$deck->{"cards"}};

        return;
    } # end Deck::Shuffle

=item give_cards(RECEIVER, NUMBER)

Transfers NUMBER cards from the donor (the object on which this method was
called) to the CardSet RECEIVER.  This method can used for dealing cards from
a deck, giving cards to another player (Go Fish), putting cards on the table
(War), or transferring a card or cards between piles in solitaire.

If NUMBER is "all", then the donor gives all of its cards.

Returns 1 usually. If the donor has too few cards, it returns 0 and does not
transfer any cards.

=cut

    sub give_cards {
    #TODO if called with a subref instead of a scalar, then sort the
    #cards to the top of the Set using the sub, and then set $number!
	my ($donor, $receiver) = (shift, shift);
	my $number = shift;
	$number = $donor->size if $number eq "all";

	# Remove the cards if we can
	if ($donor->size < $number) {
	    #print $donor->{"name"} . " is out of cards\n";
	    return 0;
	}
	my $cards_ref = $donor->remove_cards($number);
	#print $donor->{"name"}, " gives ";
	#print map {$_->print("short")} @$cards_ref;
	#print " to ", $receiver->{"name"}, "\n";

	# If we're going from a stack to a queue, we need to flip
	# the stack of cards over. E.g. if you deal three cards from
	# the stock to the waste pile in Solitaire, the top card of the
	# stock becomes the *bottom* card of the waste
	@$cards_ref = reverse @$cards_ref 
	    if $donor->{"FIFO"} != $receiver->{"FIFO"};

	# Add the cards
	$receiver->add_cards($cards_ref);

	# Store undo information
	Games::Cards::Undo->store("give", 
	                         {"donor" => $donor,
				  "receiver" => $receiver,
				  "number" => $number,
				  }
			      );

        return 1;
    } # ends sub CardSet::give_cards

=item top_card

Returns the top Card in the CardSet (or 0 if CardSet is empty)

=cut

    sub top_card {
	my $set = shift;
        return $set->size ? ${$set->{"cards"}}[-1] : 0;
    } # end sub Games::Cards::CardSet::top_card

=item face_down

Makes a whole CardSet face down

=cut

    sub face_down {
        foreach (@{shift->{"cards"}}) {$_->face_down}
    } # end sub Games::Cards::CardSet::face_down

=item face_up

Makes a whole CardSet face up

=cut

    sub face_up {
        foreach (@{shift->{"cards"}}) {$_->face_up}
    } # end sub Games::Cards::CardSet::face_up

=item print(LENGTH)

Returns a string containing a printout of the Cards in the CardSet. Prints
a long printout if LENGTH is "long", short if "short" (or nothing).
The CardSet is printed out so that the top card of the set is printed
first.

=cut

    sub print {
	my $set = shift;
	my $length = shift;
	my $long = $length && $length eq "long";
	my $max_per_line = 10;
	my $i = 0;
	my $to_print = "";
	#print $set->{"name"}." has " . scalar(@{$set->{"cards"}}) . " cards\n";

	$to_print .= $set->{"name"} . ":" . ($long ? "\n" : " ");

	# Print. Use "reverse" to print the top card of the Set first
	# (makes for easier reading when lists are long, since you usually
	# care more about the next card to be played)
        foreach my $card (reverse @{$set->{"cards"}}) {
	    $to_print .= $card->print($length);
	    if ($long) {
		$to_print .= "\n";
	    } else { # short printout
		if (++$i % $max_per_line) {
		    $to_print .= " ";
		} else {
		    $to_print .= "\n";
		    $to_print .= " " x (length($set->{"name"}) + 1);
		}
	    } # end if (short or long printout?)
	}
	# Or, if there are no cards...
	$to_print .= "(none)" unless $set->size;

	# Always print \n at end, but don't print 2
	chomp($to_print);
	$to_print .= "\n";

	return $to_print;
    } # end sub CardSet::Print

=item name

Returns the name of the Set

=cut

    sub name {return shift->{"name"}}

=item cards

Returns a reference to the array of Cards in the set

=cut

    sub cards { return shift->{"cards"}; }

=item size

Tells how many cards are in the set

=cut

    sub size {
        return scalar(@{shift->{"cards"}});
    } # end sub CardSet::size

=back

=cut

} # end package Games::Cards::CardSet

######################################################################

=head2 Class Games::Cards::Card

A Card is a playing card. Methods:

=over 4

=cut

{
package Games::Cards::Card;
# One playing card
# name is the name of the card (2-9, ace, king, queen, jack)
# value is the value of the card: e.g. ace may be 14 or 1. king may be 13 or 10.
# suit is the suit
# hidden tells whether the player can see the card

=item new(HASHREF)

creates a new card. HASHREF references a hash with keys "suit" and "name".

=cut

    sub new {
        my $class = shift;
	my $hashref = shift;
	my $card = {
	    "name" => $hashref->{"name"},
	    "suit" => $hashref->{"suit"},
	    "value" => $hashref->{"value"},
	    "face_up" => 1, # by default, you can see a card
	};

	# turn it into a playing card
	bless $card, $class;
    } # end sub Games::Cards::Card::new

=item print(LENGTH)

returns a string with the whole card name ("King of Hearts") if LENGTH is
"long", otherwise a short version ("KH").

=cut

    sub print {
	my $card = shift;
	my $length = shift;
	my $long = $length && $length eq "long";
	my ($name, $suit) = ($card->name($length), $card->suit($length));
	my $face_up = $card->{"face_up"};

	$long ? (
	    $face_up ?
		$name . " of " . $suit :
		"(Face down card)"
	    ) : ( # long
	    $face_up ?
		sprintf("%3s ", $name .  $suit) :
		"*** " 
	    )
	;

    } # end sub Card::print

=item name(LENGTH)

prints the name of the card. The full name if LENGTH is "long"; otherwise
a short version ("K");

=cut

    sub name {
        my $name = shift->{"name"};
	my $length = shift;
	my $long = $length && $length eq "long";
	
	if ($name =~ /^\d+$/) {
	   return $name;
	} else {
	   return $long ? $name : uc(substr($name, 0, 1));
	}
    } # end Games::Cards::Card::name

=item suit(LENGTH)

Returns the suit of the card. Returns the long version ("Diamonds") if LENGTH
is "long", else a short version ("D").

=cut

    sub suit { 
	my $suit = shift->{"suit"};
	my $length = shift;
	my $long = $length && $length eq "long";
        return $long ? $suit : uc(substr($suit,0,1));
    } # end sub Games::Cards::Card::suit

=item color

Is the card "red" or "black"? Returns the color or undef for unknown color.

=cut

    sub color {
        my $suit = shift->suit("long");
	my %color_map = (
	    "Diamonds" => "red",
	    "Hearts" => "red",
	    "Spades" => "black",
	    "Clubs" => "black",
	);

	if (exists ($color_map{$suit})) {
	    return $color_map{$suit};
	} else {
	    warn "unknown suit '$suit'"; 
	    return undef;
	}
    } # end sub Games::Cards::Card::color

=item value

Calculates the value of a card

=cut

    sub value { return shift->{"value"}}

=item is_face_up

Returns true if a card is face up

=cut

    sub is_face_up { return shift->{"face_up"} }

=item is_face_down

Returns true if a card is face down

=cut

    sub is_face_down { return !shift->{"face_up"} }

=item face_up

Makes a card face up

=cut

    sub face_up {
        my $card = shift;
	unless ($card->{"face_up"}) {
	    $card->{"face_up"} = 1;
	    Games::Cards::Undo->store("face_up", $card);
	}
    } # end sub Games::Cards::Card::face_up

=item face_down

Makes a card face down

=cut

    sub face_down {
        my $card = shift;
	if ($card->{"face_up"}) {
	    $card->{"face_up"} = 0;
	    Games::Cards::Undo->store("face_down", $card);
	}
    } # end sub Games::Cards::Card::face_down

=back

=cut

} # end package Card

######################################################################

=head2 Class Games::Cards::Undo

This is the package for methods to undo & redo moves. There is no CG::Undo
object. But there is a (private) array of the preceding moves (private
CG::Undo::Move objects). Methods:

=over 4

=cut

{
package Games::Cards::Undo;

# Global private variables
# CG::Undo::Undo_List holds all previous moves in CG::Undo::Move objects
# CG::Undo::Current_Move is the index of the current move in @Undo_List
# CG::Undo::Max_Size is the maximum size of the undo list
use vars qw (@Undo_List $Current_Move $Max_Size);

=item initialize(MOVES)

Initialize the Undo engine. MOVES is the number of moves to save.
0 (or no argument) allows infinite undo. This method must be called before any
undo-able moves are made (i.e., it can be called after the hands are dealt).
This method will also re-initialize the engine for a new game.

=cut

    sub initialize {
	my $class = shift;
	$Max_Size = shift || 0;
        $Current_Move = -1;
	@Undo_List = ();
    }

# Store a move in the undo stack. 
# arg0 is the type of move to store. Other args are different depending on
# the type of move.

    sub store {
        shift; # ignore class

	# Don't store moves if the undo engine hasn't been initialized
	return unless defined $Current_Move;

	my $type = shift;
	if ($type eq "give") {
	    # If we undid some moves & then do a new move instead of un_undoing,
	    # then erase the moves we undid
	    $#Undo_List = $Current_Move;

	    # Now add the move to the undo list
	    my $move = new Games::Cards::Undo::Move(@_);
	    push @Undo_List, $move;
	    shift @Undo_List if $Max_Size && @Undo_List > $Max_Size;
	    $Current_Move = $#Undo_List;
	} elsif ($type eq "face_up") {
	    $Undo_List[$Current_Move]->face_up(@_);
	} elsif ($type eq "face_down") {
	    $Undo_List[$Current_Move]->face_down(@_);
	} else { # unknown move type!
	    warn "unknown move type given to Games::Cards::Undo::store\n";
	    return;
	}
        return 1;
    } # end sub Games::Cards::Undo::store

=item undo

Undo a move

=cut

    sub undo {
	return if $Current_Move == -1;
        $Undo_List[$Current_Move--]->undo;
	return 1;
    } # end sub Games::Cards::Undo::undo


=item un_undo

Redo a move that had been undone with undo. (un_undo instead of redo, because
redo is a reserved word.)

=cut

    sub un_undo {
	return if $Current_Move == $#Undo_List;
        $Undo_List[++$Current_Move]->un_undo;
	return 1;
    } # end sub Games::Cards::Undo::un_undo

=back

=cut

{
package Games::Cards::Undo::Move;
# A CG::Undo::Move object stores one move's worth of undo information.
# This may include moving around of cards as well as face up/face down
# information.
# Fields: (not all will necessaraily have information in them)
#     donor     - the CardSet which gave cards in the move
#     receiver  - the CardSet which received cards in the move
#     cards     - the number of cards the donor gave to the receiver
#     face_up   - cards which were turned face up after the move
#     face_down - cards which were turned face down after the move
    sub new {
        my $class = shift;
	my $hashref = shift;
	my $move = {
	    "donor" => $hashref->{"donor"},
	    "receiver" => $hashref->{"receiver"},
	    "number" => $hashref->{"number"},
	    "face_up" => [],
	    "face_down" => [],
	};

	# turn it into an undo move
	bless $move, $class;
    } # end sub CG::Undo::Move::new

    sub face_down {
    # store in arg0 the fact that we turned CG::Card arg1 face down
	my ($move, $card) = @_;
        push @{$move->{"face_down"}}, $card;
    } # end sub CG::Undo::Move::face_down

    sub face_up {
    # store in arg0 the fact that we turned CG::Card arg1 face up
	my ($move, $card) = @_;
        push @{$move->{"face_up"}}, $card;
    } # end sub CG::Undo::Move::face_down

    sub undo {
    # undo move arg0
        my $move = shift;

	# Undo the give_cards portion of the move
	my ($donor, $receiver, $number) = 
	    map {$move->{$_}} ("donor", "receiver", "number");
	my $cards_ref = $receiver->un_add_cards($number);
	@$cards_ref = reverse @$cards_ref 
	    if $donor->{"FIFO"} != $receiver->{"FIFO"};
	$donor->un_remove_cards($cards_ref);

	# Un-Turn cards face up/down
	foreach (@{$move->{"face_up"}}) {$_->face_down}
	foreach (@{$move->{"face_down"}}) {$_->face_up}
    } # end sub CG::Undo::Move::undo

    sub un_undo {
    # un_undo move arg0
	my $move = shift;

	# Redo the give_cards portion of the move
	my ($donor, $receiver, $number) = 
	    map {$move->{$_}} ("donor", "receiver", "number");
	my $cards_ref = $donor->remove_cards($number);
	@$cards_ref = reverse @$cards_ref 
	    if $donor->{"FIFO"} != $receiver->{"FIFO"};
	$receiver->add_cards($cards_ref);

	# Turn cards face up/down again
	foreach (@{$move->{"face_up"}}) {$_->face_up}
	foreach (@{$move->{"face_down"}}) {$_->face_down}
    } # end sub CG::Undo::Move::un_undo

} # end package Games::Cards::Undo::Move

} # end package Games::Cards::Undo

1; # end package Games::Cards

__END__

=pod

=head1 EXAMPLES

An implementation of Klondike (aka standard solitaire) demonstrates how to use
this module in a simple game.

=head1 NOTES

=head2 TODO

=over 4

=item *

method to arrange a set, e.g. sort it by suit and value within the suit.

=item *

give_cards method that gives specific card(s) rather than the top
card(s) in the set. Or cards that match a certain criterion (e.g. all 8's).
Or a part of a column in solitaire (all cards from a certain card to the top)

=back

=head2 Maybe TODO

=over 4

=item *

Hand class that doesn't care about FIFO? I.e. it's not a Stack or a Queue
because there's no concept of the top card. You're only allowed to use
the fancier give_cards method, where you list specific cards to give.

=item *

Make various classes and methods private

=item *

Clone/deep copy method for a CardSet. This would allow e.g. saving a deck
to restart the same game of solitaire.

=item *

Move Undo to Undo.pm? Use EXPORT in that file for undo & un_undo? (Or is "undo"
going to clobber another undo method? (call it undo_move?))

=back

=head2 Not TODO

Computer AI and GUI display are left as exercises for the reader.

=head1 BUGS

Probably. This is pre-alpha.

=head1 AUTHOR

Amir Karger, karger@post.harvard.edu

=head1 SEE ALSO

perl(1).

=cut
