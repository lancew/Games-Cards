#!/usr/bin/perl -w
#
# Games::Cards - Package for card games
#
# Copyright 1999 Amir Karger (karger@post.harvard.edu)
#
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

# TODO get rid of size, have cards return wantarray ? array of cards : size
#
# TODO write Undo::Sort?

package Games::Cards;

use strict;
use vars qw($VERSION);
require 5.004; # I use 'foreach my'

# Stolen from `man perlmod`
$VERSION = do { my @r = (q$Revision: 1.34 $ =~ /\d+/g); sprintf "%d."."%02d" x $#r, @r }; # must be all one line, for MakeMaker

# sub-packages
{ 
package Games::Cards::Game;

package Games::Cards::Queue;
package Games::Cards::Stack;
package Games::Cards::Pile;
package Games::Cards::Hand;
package Games::Cards::CardSet;

package Games::Cards::Card;

package Games::Cards::Undo;
package Games::Cards::Undo::Atom;
package Games::Cards::Undo::Splice;
package Games::Cards::Undo::Face;
package Games::Cards::Undo::End_Move;
}

=pod

=head1 NAME

Games::Cards -- Perl module for playing card games

=head1 SYNOPSIS

    use Games::Cards;
    my $Rummy = new Games::Cards::Game;

    # Create and shuffle the deck and create the discard pile
    my $Deck = $Rummy->create_deck; # creates correct deck for this game
    $Deck->shuffle;
    my $Discard = new Games::Cards::Queue "Discard Pile";

    # Deal out the hands
    foreach my $i (1 .. 3) {
	my $hand = new Games::Cards::Hand "Player $i" ;
	$Deck->give_cards($hand, 7);
	$hand->sort_by_value;
	push @Hands, $hand;
    }

    # print hands (e.g. "Player 1: AS  2C  3C  3H 10D  QS  KH")
    foreach (@Hands) { print ($_->print("short"), "\n") }
    
    $Hands[1]->give_a_card ($Discard, "8D"); # discard 8 of diamonds
    
    # Undo stuff (e.g. for solitaire, not rummy)
    Games::Cards::Undo->initialize(100); # Make undo engine to save 100 moves
    Games::Cards::Undo->undo; # undo last move
    Games::Cards::Undo->redo; # redo last undone move
    Games::Cards::Undo->end_move; # tell undo engine we're done with a move

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
a name, suit, value, and suit value. The makeup of the deck was stipulated by
the 'new Games::Cards' command. Returns the deck.

=cut

    sub create_deck {
	my $game = shift;
	my $deck = new Games::Cards::Queue "Deck";
	my %cards = %{$game->{"cards_in_suit"}};

	# make an unshuffled deck
	foreach my $suit_value (1..@{$game->{"suits"}}) {
	    my $suit = $game->{"suits"}->[$suit_value-1];
	    foreach my $name (keys %cards) {
		my $new_card = new Games::Cards::Card {
		    "suit"=>$suit, "name"=> $name,
		    "suit_value" => $suit_value, "value" => $cards{$name}
		    };
		push @{$deck->{"cards"}}, $new_card;
	    }
	}

	return $deck;
    } # end sub Games::Cards::Game::create_deck

} # end package Games::Cards::Game

######################################################################
# CardSet and its subclasses

=head2 Class Games::Cards::Queue

A Queue (cf. computer science terminology, or the C++ stdlib) is a first-in
first-out pile of cards. Cards are removed from the top of the pile, but new
cards are added to the bottom of the pile.  This might represent, say, a pile
of face-down cards, like the player's hand in War.

=cut

{
package Games::Cards::Queue;
# cards array has 0 as the top card, -1 as the bottom card (opposite of Queue,
# for convenience when moving cards from a Queue to a stack or vice versa).
# We push to add cards, but shift to remove cards.
    @Games::Cards::Queue::ISA = qw(Games::Cards::Pile);

    # inherit SUPER::new

    sub remove_cards {
    # remove (and return a ref to) top arg1 cards from the Queue
	my ($thing, $number) = @_;
	return $thing->splice (0, $number);
    } # end sub Games::Cards::Queue::remove_cards

    sub add_cards {
    # Add array of Cards arg1 to the Queue
	my ($thing, $cards) = @_;
	$thing->splice ($thing->size, 0, $cards);
    } # end sub Games::Cards::Queue::add_cards

    sub top_card {
	my $set = shift;
        return $set->size ? $set->{"cards"}->[0] : 0;
    } # end sub Games::Cards::Queue::top_card

    sub print_ordered_cards {
    # returns the cards in the set in the correct order to be printed
        return shift->{"cards"};
    } # end sub Games::Cards::Queue::print_ordered_cards

} #end package Games::Cards::Queue

=head2 Class Games::Cards::Stack

A stack (cf. computer science terminology, or the C++ stdlib) is a last-in
first-out pile of cards. Cards are removed from the top of the pile, and new
cards are also added to the top of the pile. This would usually represent a
pile of cards with its top card (and perhaps all cards) face up.

=cut

{
package Games::Cards::Stack;
# cards array has -1 as the top card, 0 as the bottom card (opposite of Queue,
# for convenience when moving cards from a Queue to a stack or vice versa).
# We only access the top of the stack, pushing to add and popping to remove.
    @Games::Cards::Stack::ISA = qw(Games::Cards::Pile);

    # inherit SUPER::new

    sub remove_cards {
    # remove (and return a ref to) top arg1 cards from the Stack
	my ($thing, $number) = @_;
	return $thing->splice (-$number);
    } # end sub Games::Cards::Stack::remove_cards

    sub add_cards {
    # Add array of Cards arg1 to the Stack
	my ($thing, $cards) = @_;
	$thing->splice($thing->size, 0, $cards);
    } # end sub Games::Cards::Stack::add_cards

    sub top_card {
	my $set = shift;
        return $set->size ? $set->{"cards"}->[-1] : 0;
    } # end sub Games::Cards::Stack::top_card

    # Use "reverse" to print the top card of the Set first
    # (makes for easier reading when lists are long, since you usually
    # care more about the next card to be played)
    sub print_ordered_cards {
    # returns the cards in the set in the correct order to be printed
        return [reverse (@{shift->{"cards"}})];
    } # end sub Games::Cards::Queue::print_ordered_cards

} #end package Games::Cards::Stack

#####################

=head2 Class Games::Cards::Pile

A Pile is a pile of cards. That is, it is a CardSet where we will only access
the beginning or end of the set. (This may include the first N cards in the
set, but we will never reference the 17'th card.) This is a super class of
Queue and Stack, and those classes should be used instead, so that we know
whether the cards in the pile are FIFO or LIFO. Methods:

=over 4

=cut

{
package Games::Cards::Pile;
# The cards array is LIFO for the Stack subclass and FIFO for the Queue
# subclass. We always push things onto Queues or Stacks, but
# we use "pop", for Stacks, and "shift" for the Queues.

    @Games::Cards::Pile::ISA = qw(Games::Cards::CardSet);
    # inherit SUPER::new

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

    # If we're going from a Stack to a Queue, we  would normally need to flip
    # the stack of cards over. E.g. if you deal three cards from the stock to
    # the waste pile in Solitaire, the top card of the stock becomes the
    # *bottom* card of the waste. However, the cards arrays in Stacks and
    # Queues are stored in opposite directions, so this works automatically!
    #    If we're giving to a Hand, which doesn't have a top card, it doesn't
    # matter

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

	# Add the cards
	$receiver->add_cards($cards_ref);

        return 1;
    } # end sub Games::Cards::Pile::give_cards

=item top_card

Returns the top Card in the CardSet (or 0 if CardSet is empty)

=cut

    # This sub is actually found in the subclasses, since their
    # arrays are stored in different orders
} #end package Games::Cards::Pile

#####################

=head2 Class Games::Cards::Hand

A Hand represents a player's hand. Most significantly, it's a CardSet which
is different from a Pile because the Cards in it are unordered. We may
reference any part of the CardSet, not just the top or bottom.
Methods:

=over 4

=cut

{
package Games::Cards::Hand;

    @Games::Cards::Hand::ISA = qw(Games::Cards::CardSet);
# Use SUPER::new

=item give_a_card(RECEIVER, DESCRIPTION)

Transfers Card described by DESCRIPTION from the donor (the object on which
this method was called) to the CardSet RECEIVER.  This method can used for
discarding a card from a hand, e.g. 

If DESCRIPTION matches /^-?\d+$/, then it is the index in the cards array of the
Card to give.  Otherwise, DESCRIPTION is passed to Hand::index. 

Returns 1 usually. If the donor does not have the card, it returns 0 and does
not transfer anything.

=cut

    sub give_a_card {
	my ($donor, $receiver) = (shift, shift);
	my $description = shift;

	# Which card to remove?
	my $donor_index = $description =~ /^-?\d+$/ ?
	                  $description :
			  $donor->index($description);

	unless (defined $donor_index && $donor_index < $donor->size) {
	    #print $donor->name . " does not have that card\n";
	    return;
	}

	# Remove the card
	my $card_ref = $donor->remove_a_card($donor_index);
	#print $donor->name, " gives ";
	#print map {$_->print("short") . " "} @$cards_ref;
	#print " to ", $receiver->name, "\n";

	# Add the card
	$receiver->add_cards([$card_ref]); # add_cards takes an array ref

        return 1;
    } # end sub Games::Cards::Hand::give_card

=item move_card(DESCRIPTION, INDEX)

Rearrange a Hand by putting Card described by DESCRIPTION at index INDEX.

If DESCRIPTION matches /^-?\d+$/, then it is the index in the cards array of the
Card to give.  Otherwise, DESCRIPTION is passed to Hand::index. 

Returns 1 usually. If the donor does not have the card, it returns 0 and does
not transfer anything.

=cut

    sub move_card {
        my $hand = shift;
	my ($description, $final) = @_;

	# Which card to remove?
	my $initial = $description =~ /^-?\d+$/ ?
		      $description :
		      $hand->index($description);

	# don't have that card!
	return unless defined $initial;

	# Remove the card
	my $card_ref = $hand->remove_a_card($initial);

	# Add the card
	$hand->add_a_card($card_ref, $final);

        return 1;
    } # end sub Games::Cards::Hand::move_card

    sub remove_a_card {
    # remove (and return a ref to an array with) card number arg1 of the Hand
	my ($thing, $number) = @_;
	# splice returns an array ref
	my $listref = $thing->splice ($number,1);
	return $listref->[0];
    } # end sub Games::Cards::Stack::remove_cards

    sub add_a_card {
    # add card arg1 at position arg2 number arg1 of the Hand arg0
	my ($thing, $card, $number) = @_;
	$thing->splice ($number,0,[$card]);
    } # end sub Games::Cards::Stack::remove_cards

    sub add_cards {
    # Add array of Cards arg1 to the Hand
    #    This sub is called by Pile::give_cards & doesn't care where in the
    # Hand the cards end up. So just put 'em at the end
	my ($thing, $cards) = @_;
	$thing->splice($thing->size, 0, $cards);
    } # end sub Games::Cards::Hand::add_cards

=item index(DESCRIPTION)

Given a card description DESCRIPTION return the index of that Card
in the Hand, or undef if it was not found. DESCRIPTION may be a Card or
a string (like "8H" or "KC").

=cut 

    sub index {
	# Depending on the nature of the description arg1, we create a sub
	# to match that description with a Card. Then we search among the
	# cards in Hand arg0's cards array with that sub
	my ($set, $description) = @_;
	my $number;
	my $find; # sub whose arg0 is a card to compare to 

	if (ref $description eq "Games::Cards::Card") {
	    $find = sub {shift == $description};

	# Note: This is a bad way to match card names!
	# TODO methods to break a card description apart
	# but it matches 2-10 or AKQJ of CHDS
	} elsif ($description =~ /^[\dakqj]+[chds]/i) {
	    $find = sub {(my $pr = shift->print("short")) =~ s/\s*//g;
	                 $pr eq uc($description)};
	} else {
	    my $caller = (caller(0))[3];
	    die "$caller called with unknown description $description\n";
	}

	foreach my $i (0..$#{$set->{"cards"}}){
	    my $card = $set->{"cards"}->[$i];
	    $number = $i if &$find($card);
	}

	return $number; # will return undef if card wasn't found
    }

    sub print_ordered_cards {
    # returns the cards in the set in the correct order to be printed
        return shift->{"cards"};
    } # end sub Games::Cards::Hand::print_ordered_cards

} #end package Games::Cards::Hand

##################

=head2 Class Games::Cards::CardSet

A CardSet is just an array of cards (stored in the "cards" field). It could be
a player's hand, a deck, or a discard pile, for instance. This is a super class
of a number of other classes, and those subclasses should be used instead.

=over 4

=cut

#####################

{
package Games::Cards::CardSet;
# Fields:
# cards - array of Cards name - "Joe's Hand" for Joe's hand, "discard" for a
# discard pile, etc.

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

    # Splice cards into/out of a set
    # Just like Perl's splice (with different argument types!)
    # RESULT = splice(ARRAY, OFFSET, LENGTH, LIST);
    # ARRAY is a CardSet, 
    # OFFSET is the index in the "cards" array
    # LENGTH is the number of cards spliced out,
    # LIST is a reference to an array of Cards to splice in
    # RESULT is (empty or) a ref to an array of Cards that were spliced out
    # (LENGTH and LIST are optional)
    #
    # This sub is private. People should use add_cards et al., which call
    # this sub
    sub splice {
	my ($set, $offset, $length, $in_cards) = @_;
	# set in_cards to empty list if undef. Otherwise, we'd splice in (undef)
	$in_cards = [] unless defined $in_cards;

	# Negative offsets will break if we try to undo them
	$offset += $set->size if $offset < 0;

	# If we didn't get length, splice to end of array
	$length = $set->size - $offset unless defined $length;
	# print $set->name, ": ",$set->size,
	#    " cards - $length starting at $offset",
	#    " + ", scalar(@$in_cards)," = ";

	# Can't splice in past position #$cards+1==foo->size
	# Can't splice out more cards than we have
	warn "illegal splice!\n" if $offset > $set->size || 
				    $length + $offset > $set->size;

	# Do the splice
	my $out_cards = [splice (@{$set->{"cards"}}, $offset,
	                         $length, @$in_cards)];

	# Store the splice & its result for Undo
	my $atom = new Games::Cards::Undo::Splice {
			"set" => $set,
			"offset" => $offset,
			"length" => $length,
			"in_cards" => $in_cards,
			"out_cards" => $out_cards,
			};
	$atom->store; # store the atom in the Undo List

	# print $set->size,"\n";
	return $out_cards;
    } # end sub Cards::Games::splice

=item shuffle

shuffles the cards in the CardSet. Shuffling is not undoable.

=cut

    sub shuffle {
    # shuffle the deck (or subset thereof)
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
    } # end sub CardSet::Shuffle

=item sort_by_value

Sorts the Set by value. This and other sort routines will probably be used
mostly on Hands, which are "ordered sets", but you might want to reorder a deck
or something. Sorting is not undoable.

=item sort_by_suit

Sorts the Set by suit, but not by value within the suit.

=item sort_by_suit_and_value

Sorts the Set by suit, then by value within the suit.

=cut

    sub sort_by_value {
        my $set = shift;
	@{$set->{"cards"}} = sort {$a->value <=> $b->value} @{$set->{"cards"}}
    } # end sub Games::Cards::CardSet::sort_by_value

    sub sort_by_suit {
        my $set = shift;
	@{$set->{"cards"}} =  sort {$a->suit_value <=> $b->suit_value} 
			           @{$set->{"cards"}}
    } # end sub Games::Cards::CardSet::sort_by_suit

    sub sort_by_suit_and_value {
        my $set = shift;
	@{$set->{"cards"}} = sort {$a->suit_value <=> $b->suit_value ||
	                           $a->value <=> $b->value} 
				@{$set->{"cards"}}
    } # end sub Games::Cards::CardSet::sort_by_suit_and_value

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
The CardSet is printed out in reverse order, so that the top card of the set is
printed first.

=cut

    sub print {
	my $set = shift;
	my $length = shift;
	my $long = $length && $length eq "long";
	my $max_per_line = 10;
	my $i = 0;
	my $to_print = "";
	#print $set->{"name"}." has " . $set->size . " cards\n";

	$to_print .= $set->{"name"} . ":" . ($long ? "\n" : " ");

	# Print. Different types of Sets are printed in different order
        foreach my $card (@{$set->print_ordered_cards}) {
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

    sub size { return scalar(@{shift->{"cards"}}); }

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
# suit_value is the value of the suit: e.g. in bridge spades is 4, clubs 1
#  (although that may change after bidding!)
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
	    "suit_value" => $hashref->{"suit_value"},
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
	    return;
	}
    } # end sub Games::Cards::Card::color

=item value

Calculates the value of a card

=cut

    sub value { return shift->{"value"}}

=item suit_value

Returns the suit_value of a card (1..number of suits)

=cut

    sub suit_value { return shift->{"suit_value"}}

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
	    my $atom = new Games::Cards::Undo::Face {
			    "card" => $card,
			    "direction" => "up",
			    };
	    $atom->store; # store the atom in the Undo List
	}
    } # end sub Games::Cards::Card::face_up

=item face_down

Makes a card face down

=cut

    sub face_down {
        my $card = shift;
	if ($card->{"face_up"}) {
	    $card->{"face_up"} = 0;
	    my $atom = new Games::Cards::Undo::Face {
			    "card" => $card,
			    "direction" => "down",
			    };
	    $atom->store; # store the atom in the Undo List
	}
    } # end sub Games::Cards::Card::face_down

=back

=cut

} # end package Card

######################################################################

=head2 Class Games::Cards::Undo

This is the package for methods to undo & redo moves. There is no CG::Undo
object. But there is a (private) array of the preceding moves (private
CG::Undo::Move objects). Note that a "move" is made up of several "atoms".
For example, moving a card from one column to another in solitaire involves
one or more Splice atoms (removing or adding card(s) to a CardSet) and 
possibly a Face atom (turning a card over).

Generally, these methods will be called by other
Games::Cards methods, but not by the actual games. Methods:

=over 4

=cut

{
package Games::Cards::Undo;
# How does Games::Cards handle undo?
#
# Undo_List is just an array of (objects from derived classes of) Undo::Atoms.
# E.g. in solitaire one "move" might include moving cards from one column to
# another (two Undo::Splice objects) and turning a card over (a Undo::Face
# object) The undo list will store those Atoms as well as an End_Move object,
# which is just a placeholder saying that move is over.

# Global private variables
# Can't keep this info in an object, because private GC subroutines
# (like CardSet::splice) need access to the Undo list, and I shouldn't have
# to pass the undo object around to every sub.
# GC::Undo::Undo_List holds all previous moves in GC::Undo::Atom objects
# GC::Undo::Current_Atom is the index of the current Atom in @Undo_List
# GC::Undo::Max_Size is the maximum size (moves, not Atoms!) of the undo list
# GC::Undo::In_Undo says that we're currently doing (or undoing) an Undo, so we
# shouldn't store undo information when we move cards around
my (@Undo_List, $Current_Atom, $Max_Size, $In_Undo);

=item new(MOVES)

Initialize the Undo engine. MOVES is the number of atoms to save.
0 (or no argument) allows infinite undo.

This method must be called before any undo-able moves are made (i.e., it can be
called after the hands are dealt).  This method will also re-initialize the
engine for a new game.

=cut

    sub new {
	my $class = shift;
	# (re)set global private variables
	$Max_Size = shift || 0;
        $Current_Atom = -1;
	@Undo_List = ();
	$In_Undo = 0;

	# Make the (dummy) object to give a "handle" for methods
	my $thing = {};
	bless $thing, $class;
	return $thing;
    }

=item end_move

End the current move. Everything between the last call to end_move and now
is considered one move. This tells undo how much to undo.

=cut

    sub end_move {
	# calling with just "store(foo)" there aren't enough args!
	my $atom = new Games::Cards::Undo::End_Move;
	$atom->store;
    } # end sub Games::Cards::Undo::end_move

    sub store {
    # Stores a move in the undo list, which can later be undone or redone. The
    # first argument is the type of move to store, other args give details about
    # the move depending on the move type.
    #
    # arg1 is a subclass of Undo::Atom
	# Don't store moves if the undo engine hasn't been initialized
	return unless defined $Current_Atom;

	# don't store undo moves when we're currently implementing an undo/redo
	return if $In_Undo; 

        shift; # ignore class
	my $atom = shift; # the Undo::Atom to store

	# If we undid some moves & then do a new move instead of redoing,
	# then erase the moves we undid
	$#Undo_List = $Current_Atom;

	# Now add the move to the undo list
	push @Undo_List, $atom;

	# If the list is too big, remove a whole move (not just an Atom)
	# from the beginning of the list (oldest undos)
	my $end_class = "Games::Cards::Undo::End_Move";
	if ($Max_Size && grep {ref eq $end_class} @Undo_List > $Max_Size) {
	    $atom = shift @Undo_List until ref($atom) eq $end_class;
	}

	$Current_Atom = $#Undo_List;

        return 1;
    } # end sub Games::Cards::Undo::store

=item undo

Undo a move.

=cut

    sub undo {
    # undoing a move means undoing all the Atoms since the last
    # End_Move Atom
    # Note that this sub can (?) also undo from the middle of a move
	# If called w/ class instead of object, and we never called new(),
	# then return. This shouldn't happen.
	return unless defined $Current_Atom; # never called new
	return if $Current_Atom == -1;
	$In_Undo = 1; # Don't store info when moving cards around

	# Loop until the next End_Move Atom or until we exhaust the undo list
	my $end_class= "Games::Cards::Undo::End_Move";
	$Current_Atom-- if ref($Undo_List[$Current_Atom]) eq $end_class;
	for (;$Current_Atom > -1; $Current_Atom--) {
	   my $atom = $Undo_List[$Current_Atom];
	   last if ref($atom) eq $end_class;
	   $atom->undo;
	}
	# now $Current_Atom is on the End_Move at the end of the last move

	$In_Undo = 0; # done undoing. Allowed to store again.
	return 1;
    } # end sub Games::Cards::Undo::undo


=item redo

Redo a move that had been undone with undo.

=cut

    sub redo {
    # redoing a move means redoing every Atom from the current atom
    # (which should be an End_Move) until the next End_Move atom
	# If called w/ class instead of object, and we never called new(),
	# then return. This shouldn't happen.
	return unless defined $Current_Atom; 
	return if $Current_Atom == $#Undo_List;
	$In_Undo = 1; # Don't store info when moving cards around

	# Loop until the next End_Move Atom or until we exhaust the undo list
	my $atom;
	my $end_class = "Games::Cards::Undo::End_Move";
	$Current_Atom++ if ref($Undo_List[$Current_Atom]) eq $end_class;
	for (;$Current_Atom <= $#Undo_List; $Current_Atom++) {
	   my $atom = $Undo_List[$Current_Atom];
	   last if ref($atom) eq $end_class;
	   $atom->redo;
	}
	# now $Current_Atom is on the End_Move at the end of this move

	$In_Undo = 0; # done redoing. Allowed to store again.
	return 1;
    } # end sub Games::Cards::Undo::redo

=back

=cut

{
package Games::Cards::Undo::Atom;
# A CG::Undo::Atom object stores the smallest indivisible amount of undo
# information. The subclasses of this class implement different kinds of atoms,
# as well as the way to undo and redo them.

    sub new {
    # This new will be used by subclasses
    # arg0 is the class. arg1 is a hashref containing various fields. Just
    # store 'em.
        my $class = shift;
	my $atom = shift || {};

	# turn it into an undo move
	bless $atom, $class;
    } # end sub Games::Cards::Undo::Atom::new

    sub store {
    # Store this Atom in the Undo List
        Games::Cards::Undo->store(shift);
    } # end sub Games::Cards::Undo::Atom::store

} # end package Games::Cards::Undo::Atom

{
package Games::Cards::Undo::End_Move;
# An Undo::End_Move is just a marker. Everything in the Undo_List from just
# after the last End_Move until this one is one "move". 

    @Games::Cards::Undo::End_Move::ISA = qw(Games::Cards::Undo::Atom);

    # inherit SUPER::new
    # No other methods necessary!

} # end package Games::Cards::Undo::End_Move

{
package Games::Cards::Undo::Face;

    @Games::Cards::Undo::Face::ISA = qw(Games::Cards::Undo::Atom);

    # inherit SUPER::new

    sub undo {
        my $face = shift;
	my ($card, $direction) = ($face->{"card"}, $face->{"direction"});
	if ($direction eq "up") {
	    $card->face_down;
	} elsif ($direction eq "down") {
	    $card->face_up;
	} else {
	    my $func = (caller(0))[3];
	    die ("$func called with unknown direction $direction\n");
	}
    } # end sub Games::Cards::Undo::Face::undo

    sub redo {
        my $face = shift;
	my ($card, $direction) = ($face->{"card"}, $face->{"direction"});
	if ($direction eq "up") {
	    $card->face_up;
	} elsif ($direction eq "down") {
	    $card->face_down;
	} else {
	    my $func = (caller(0))[3];
	    die ("$func called with unknown direction $direction\n");
	}
    } # end sub Games::Cards::Undo::Face::redo

} # end package Games::Cards::Undo::Face

{
package Games::Cards::Undo::Splice;

    @Games::Cards::Undo::Splice::ISA = qw(Games::Cards::Undo::Atom);

    # inherit SUPER::new

    sub undo {
    # If we changed ARRAY by doing:
    # RESULT = splice(ARRAY, OFFSET, LENGTH, LIST);
    # then we can return ARRAY to its original form by
    # splice(ARRAY, OFFSET, scalar(LIST), RESULT);
    #
    # (sub splice also made sure that for calls to splice without 
    # all the arguments, the missing arguments were added, and that OFFSET
    # would be >= 0)

	my $splice = shift;
	# Could do this quicket with no strict refs :)
	my ($set, $offset, $in_cards, $out_cards) = 
	    map {$splice->{$_}} qw(set offset in_cards out_cards);

	# Do the anti-splice and return its return value
	# (Return will actually be in_cards!)
	$set->splice ($offset, scalar(@$in_cards), $out_cards);
    } # end sub Cards::Games::Undo::Splice::undo

    sub redo {
    # we changed ARRAY by doing:
    # RESULT = splice(ARRAY, OFFSET, LENGTH, LIST);
    # Just redo the splice.
    # (sub splice also made sure that for calls to splice without 
    # all the arguments, the missing arguments were added, and that OFFSET
    # would be >= 0)

	my $splice = shift;
	# Could do this quicket with no strict refs :)
	my ($set, $offset, $in_cards, $length) = 
	    map {$splice->{$_}} qw(set offset in_cards length);

	# Do the splice and return its return value
	# (Return will actually be out_cards!)
	$set->splice ($offset, $length, $in_cards);
    } # end sub Cards::Games::Undo::Splice::redo

} # end package Games::Cards::Undo::Splice

} # end package Games::Cards::Undo

1; # end package Games::Cards

__END__

=pod

=head1 EXAMPLES

An implementation of Klondike (aka standard solitaire) demonstrates how to use
this module in a simple game. Other card game examples are included as well.

=head1 NOTES

=head2 Public and Private

This module contains a bunch of methods. The public methods are documented
here. That means any method I<not> documented here is probably private, which
means you shouldn't call it directly.

There are also a bunch of classes. Most private classes are not documented
here. A couple private classes are mentioned, since they have methods which
public classes inherit. In that case, the privateness is mentioned.

=head2 TODO

=over 4

=item *

Networking games, so that multiple players on different computers can play 
games (using sockets). This is currently under active development. (gin.pl has
already been written, and it should require very little changing, other than
'use Games::Cards::Server' and a bit more I/O.

=item *

ExtraCards. In addition to Suits * Cards_In_Suit, have this field of cards
added to the deck. E.g., jokers. (Or a "color card" for a dealer's card shoe.)
These probably need values too.

=item *

Allow user to change more things. E.g. create an AUTOLOAD which returns
that field, or changes it if there's an argument (cf. Cookbook).

=back

=head2 Maybe TODO

=over 4

=item *

CGI support should be easy to add, so people could play multiplayer games in
their web browsers. That is further in the future than socket-based
Client/Server games, however.

=item *

Clone/deep copy method for a CardSet. This would allow e.g. saving a deck
to restart the same game of solitaire.

=back

=head2 Not TODO

Computer AI and GUI display are left as exercises for the reader.

=head1 BUGS

You betcha. It's still alpha.

=head1 AUTHOR

Amir Karger, karger@post.harvard.edu

=head1 SEE ALSO

perl(1).

=cut
