#!/usr/bin/perl -w

# klondike.pl - Play Klondike (standard solitaire)
#
# Copyright 1999 Amir Karger (karger@post.harvard.edu)
#
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.
#

use strict;
use Games::Cards;

srand();

######################################################################
# SETUP THE GAME
# Main variables
my $Klondike; # the game object
my $Save_Deck; # $Deck is copied from this; used for restart
my $Deck; # the deck we're using in the game
my %Foundations; # the four piles we're trying to fill
my @Tableau; # the table, where most of the play happens
my $Tableau_Size = 7; # number of piles in tableau
my $Stock; # cards in our hand
my $Waste; # cards go from stock to waste
my $Cards_From_Stock = 3; # how many stock cards to take at a time

my $Error; # current error message
my $Usage =<<"ENDUSAGE";
  $0 - play "klondike", aka standard solitaire

  - Columns are 1 to 7, plus the Waste pile, Foundations and Stock

  - Try to build piles of Ace through King of each suit in the Foundations.

  - You can move the top card from the Waste pile onto the
  corresponding Foundation pile or onto another column. Alternatively, you
  can move all or a portion of a column onto another column.

  - You can move a card onto the next highest card of a different color, so,
  e.g. a red ten can go on a black Jack. Only a King can be moved onto an
  empty column.

  - Take cards 3 at a time from the Stock into the Waste pile.
  
  - Commands are one or two letters or numbers (from 1 to 7)

    23 moves a card from column 2 to 3
    2f moves a card from column 2 to the foundation pile of the right suit
    wf moves a card from the waste pile to the foundation pile of the right suit
    w2 moves a card from the waste pile to column 2
    ws moves the whole waste pile back into the stock
    sw (or just s) moves from the stockpile to the waste pile
    finish attempts to finish the game. The stock & wastepile must be empty.

    u  undo last move (multiple undo/redo works)
    r  redo the last move you undid
    restart   start the game over with the same deck

    q  quits a game, allowing you to start a new round or stop playing entirely
    h  prints this help
ENDUSAGE

# Create the game.
# Use the default deck (standard suits, cards, & card values)
$Klondike = new Games::Cards::Game;

# Create and shuffle the deck
print "Creating new deck.\n";

NEWGAME: # We go to here when starting a new game
$Save_Deck = $Klondike->create_deck("Save Deck");
print "Shuffling the deck.\n";
$Save_Deck->shuffle;

RESTART: # We go here when we restart a game
$Deck = $Save_Deck->clone("Deck");

# Deal out the Tableau
@Tableau = ();
foreach my $i (1 .. $Tableau_Size) {
    my $column = new Games::Cards::Stack("Column $i");
    $Deck->give_cards($column, $i);
    # Make sure all columns are face down, with top card face-up
    $column->face_down;
    $column->top_card->face_up;
    push @Tableau, $column;
}

# Create the empty Foundations
%Foundations = ();
foreach (@{$Klondike->{"suits"}}) {
    $Foundations{$_} = new Games::Cards::Stack(ucfirst($_) . " Pile"); 
}

# Stock has what's left in the deck, wastepile starts out empty
$Stock = new Games::Cards::Queue("Stock");
$Deck->give_cards($Stock, "all");
$Waste = new Games::Cards::Stack("Waste");

# Initialize the Undo engine with infinite size (no size arg. given to new)
my $Undo = new Games::Cards::Undo;

######################################################################
# Now play

my $turns = 1; # 1 gets subtracted later cuz Error is set
$Error = "Welcome! Type h for help, q to quit";
LOOP: while (++$turns) {

    &print_game;

    # If we got an error on the last turn, print the game status *first*, then
    # print the error right before the prompt (so that the reader will see it)
    if ($Error) {
	print "$Error\n\n";
	$turns--;
	$Error = "";
    }

    # Ask player what to do
    print "Turn $turns: ";
    my $input = <STDIN>; chomp($input);
    # Big case statement
    for ($input) {
	s/\s*//g;

	# Move top card of the waste pile to a column OR move one or
	# more cards from a column to another column
        if (/^([w\d])(\d)$/i) {
	    &move_to_column($1, $2);
	    $Undo->end_move;

	# Move a card to (correct) foundation from waste or from a column
        } elsif (/^([w\d])f$/i) {
	    &move_to_foundation($1);
	    $Undo->end_move;

	# waste to stock
	} elsif (/^ws$/i) {
	    if ($Stock->size) {
	        $Error = "ERROR! Stock isn't empty yet!";
		next;
	    }
	    $Waste->give_cards($Stock, "all");
	    $Undo->end_move;

	# stock to waste
	# Take three cards
	} elsif (/^sw?$/i) {
	    #print $Stock->print("short");
	    my $size = $Stock->size or
	        $Error = "ERROR! Stock is empty. (ws to replenish)", next;
	    # Take 3 cards at a time. But just take 2 if only two are left
	    my $number = ($size < $Cards_From_Stock) ?
	                  $size : 
			  $Cards_From_Stock;
	    $Stock->give_cards($Waste, $number);
	    $Undo->end_move;

	# finish the game?
	} elsif (/^finish/i) {
	    if ($Stock->size || $Waste->size) {
	        $Error = "ERROR! Stock and waste must be empty to finish";
		$turns--;
		next;
	    }
	    foreach my $col (@Tableau) {
	        if (grep {$_->is_face_down} $col->cards) {
		    $Error = "ERROR! All cards must be face up to finish";
		    $turns--;
		    next LOOP;
		}
	    }

	    my $save_turns = $turns;
	    my $did_move = 0;
	    do {
		$did_move = 0;
	        foreach my $j (1..@Tableau) {
		    $did_move = 1, $turns++ if &move_to_foundation($j);
		}
		&check_win($turns);
		&print_game; sleep(1);
	    } while $did_move == 1;

	    # If we got here, we didn't win
	    $Undo->end_move;
	    $Undo->undo; # undo any progress we made
	    $Error = "ERROR! Unable to finish!\n";
	    $turns = $save_turns;

	# restart
	} elsif (/^restart/i) {
	    $Undo->end_move; # doesn't really matter: $Undo will be clobbered
	    goto RESTART;

	# undo
	} elsif (/^u/i) {
	    $Undo->undo or
	        $Error = "ERROR! Can't undo any more", next;
	    # subtract the "undo" turn *and* the previous move
	    $turns-=2;

	# redo
	} elsif (/^r/i) {
	    $Undo->redo or
	        $Error = "ERROR! Can't redo any more", next;
	    # $turns=$turns; subtract the "redo" turn but add a move.

	# help
	} elsif (/^h/i) {
	    print $Usage;
	    print "\nType RETURN to continue\n";
	    <STDIN>;
	    $turns--;

	# quit game
	} elsif (/^q/i) {
	    print "Are you sure you want to quit? (y/n): ";
	    my $a = <STDIN>;
	    if ($a =~ /^\s*y/i) {
		print "Would you like to play another game? (y/n): ";
		$a = <STDIN>;
		if ($a =~ /^\s*n/i) {
		    print "Bye!\n";
		    last LOOP;
		} else {
		    goto NEWGAME;
		}
	    }
	    $turns--;

	} else {
	    $Error = "ERROR! unknown command. Try again (h for help)"
	} # end case if

	&check_win($turns);
    } # end big case statement

} #end while (loop over turns)

exit;

######################################################################
sub move_to_column {
# Move a card from Stack arg0 to column arg1
# Arg0 can be a column number or "w" for the waste pile
#
# Return 1, or 0 for error
    my ($from, $to) = @_;
    my ($donor, $receiver);

    # From which Stack are we taking cards?
    if ($from =~ /\d/) {
	$donor = $Tableau[$from-1]; # Columns are 1..n, array is 0..n-1
	die "illegal column $from is sub move_to_column\n" unless $donor;
    } elsif ($from eq "w") {
        $donor = $Waste;
    } else {die "Unknown first arg '$from' to sub move_to_column!\n";}
    unless ($donor->size) { $Error = "ERROR! No cards to move!"; return 0; }

    # To which Stack are we transferring cards?
    die "Unknown second arg '$to' to sub move!\n" unless $to =~ /\d/;
    $receiver = $Tableau[$to-1];
    unless ($receiver) { $Error = "ERROR! Illegal column $to!"; return 0; }


    # If we're going column to column, search through the (face-up cards
    # in the) column for the card that can legally move to the other column,
    # then transfer that card and all cards below it.
    #    If we're going from the waste pile to a column, just take the top card
    # and confirm that it's allowed to transfer
    my @cards;
    if ($from =~ /\d/) {
	# Reverse so that we go from the lowest number on the pile upward. This
	# allows us to count the number of cards we're transferring
        @cards = reverse(grep {$_->is_face_up} @{$donor->cards});
    } else {
        @cards = ($donor->top_card);
    }

    my $allowed = 0; # are we allowed to transfer?
    my $transferred = 0; # number of cards to transfer
    my $receiver_card = $receiver->top_card;
    foreach my $card (@cards) {
	$transferred++;
	# card must be different color & next lower card
	# Or a king can go onto an empty column
	if ($receiver_card) {
	    $allowed = ($receiver_card->value == $card->value + 1 &&
			$receiver_card->color ne $card->color);
	} else {
	    $allowed = $card->name("long") eq "King";
	}

	last if $allowed;
    }

    unless ($allowed) { $Error = "ERROR! Illegal move!"; return 0; }

    # Now actually transfer the card(s)
    $donor->give_cards($receiver, $transferred);

    # After removing a card from a column, make sure the next card in
    # that column is face up
    my $a;
    if ($from =~ /\d/ && ($a = $donor->top_card)) {$a->face_up}

    return 1;
} # end sub move_to_column

sub move_to_foundation {
# Move a card from arg0 to the correct foundation for that suit
# Arg0 can be a column number or "w" for the waste pile
#
# Return 1, or 0 for error
    my ($from) = @_;
    my ($donor, $receiver);

    # From which Stack are we taking cards?
    if ($from =~ /\d/) {
	$donor = $Tableau[$from-1]; # Columns are 1..n, array is 0..n-1
	die "illegal column $from in sub move_to_foundation\n" unless $donor;
    } elsif ($from eq "w") {
        $donor = $Waste;
    } else {warn "Unknown first arg '$from' to sub move_to_foundation!\n";}
    unless ($donor->size) { $Error = "ERROR! No cards to move!"; return 0; }

    my $donor_card = $donor->top_card;

    # To which Stack are we transferring cards?
    $receiver = $Foundations{$donor_card->suit("long")};
    die "Unknown suit in sub move_to_foundation!\n" unless $receiver;

    my $allowed = 0; # are we allowed to transfer?
    my $receiver_card = $receiver->top_card;
    if ($receiver_card) {
	$allowed = ($receiver_card->value == $donor_card->value - 1);
    } else { # empty foundation
	$allowed = $donor_card->name("long") eq "Ace";
    }

    unless ($allowed) { $Error = "ERROR! Illegal move!"; return 0; }

    # Now actually transfer the card
    $donor->give_cards($receiver, 1);

    # After removing a card from a column, make sure the next card in
    # that column is face up
    my $a;
    if ($from =~ /\d/ && ($a = $donor->top_card)) {$a->face_up}

    return 1;
} # end sub move_to_foundation

sub check_win {
    my ($turns) = shift;
    my $a;
    
    if ((grep {$a=$_->top_card and $a->name("long") eq "King"} 
        (values %Foundations)) == 4) {
	print "You have won after $turns turns!\n";
	print "\n\nWould you like to play another game? (y/n): ";
	$a = <STDIN>;
	goto NEWGAME if ($a =~ /\s*^y/i);
	exit;
    }
}

sub print_columns {
# print the columns in rows (2-D display)
# args are a list of CardSets
    my @columns = @_;
    # Print from bottom to top...
    my $index = (sort {$a <=> $b} (map {$_->size} @columns))[-1] -1;
    print " ",join("     ",(1..@columns)),"\n";
    print join("   ",("---") x @columns),"\n";
    foreach (0 .. $index) {
	my $to_print = "";
        foreach my $column (@columns) {
	    my $a = ${$column->cards}[$_];
	    my $p = defined $a ? $a->print("short") : "    ";
	    $to_print .= "$p  ";
	} # end loop over one row of each column
	print "$to_print\n";
    } # end loop over all rows
    print "\n";
} # end sub print_columns

sub print_game {
# print out the current status in solitaire
    print "\n\n\n", "-" x 50,"\n";
    print "Stock has ", $Stock->size, " cards\n";
    print $Waste->print("short"),"\n";
    print "Foundations:   ";
    foreach (keys %Foundations) {
	my $set = $Foundations{$_};
        my $to_print = $set->size ? 
	    $set->top_card->print("short") :
	    "(none)"; 
	print "$to_print   ";
    }
    print "\n\n";

    &print_columns(@Tableau);
} # end sub print_game

