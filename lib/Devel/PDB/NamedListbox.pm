# vi: set autoindent shiftwidth=4 tabstop=8 softtabstop=4 expandtab:
package Devel::PDB::NamedListbox;
use strict;
use warnings;

use Curses;
use Curses::UI::Common;
use Curses::UI::Listbox;
use Devel::PDB::Dialog::Message;

use vars qw(
  $VERSION
  @ISA
  );

@ISA = qw(
  Curses::UI::Listbox
  Curses::UI::Common
  );

$VERSION = '1.1';

my $var_name_spaces = 20;

sub new {
    my $class = shift;
    my $this  = $class->SUPER::new(@_);

    $this->set_binding(\&delete_item, KEY_DC) unless $this->{-readonly};
    $this->set_binding(\&show_item, KEY_ENTER);

    $this;
}

sub delete_item {
    my $this       = shift;
    my $id         = $this->get_active_id;
    my $named_list = $this->{-named_list};

    splice @$named_list, $id, 1;
    $this->update;
}

sub show_item {
    my $this = shift;
    my $id   = $this->get_active_id;
    my $item = $this->{-named_list}->[$id];

    Devel::PDB::Dialog::Message->run(-title => $item->{name}, -message => $item->{long_value}, %DB::def_style);
}

sub named_list {
    my ($this, $list) = @_;

    $this->{-named_list} = $list if defined $list;

    $this->{-named_list};
}

sub update {
    my ($this, $refresh) = @_;
    my $list = $this->{-named_list};
    my @display;

    if ($this->{-sort_key}) {
        my @a = sort { $a->{name} cmp $b->{name} } @$list;
        $list = \@a;
    }

    foreach my $item (@$list) {
        my $name = $item->{name};
        $name = substr($name, 0, $var_name_spaces - 2) . '..' if length $name > $var_name_spaces;
        push @display, $name . ' ' x ($var_name_spaces + 1 - length($name)) . $item->{value};
    }
    $this->{-values} = \@display;

    if ($refresh) {
        $this->clear_selection;
        $this->option_first if @display;
    }

    $this->schedule_draw(1);
}

1;
