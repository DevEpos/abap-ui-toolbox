INTERFACE zif_uitb_gui_composite_view
  PUBLIC .

  INTERFACES zif_uitb_gui_view.

  "! <p class="shorttext synchronized" lang="en">Set Child visible/hidden</p>
  "!
  METHODS set_child_visibility
    IMPORTING
      io_child   TYPE REF TO zif_uitb_gui_view
      if_visible TYPE abap_bool DEFAULT abap_true.

  "! <p class="shorttext synchronized" lang="en">Executes the given command</p>
  "!
  METHODS execute_command DEFAULT IGNORE
    IMPORTING
      io_command TYPE REF TO zif_uitb_gui_command.
ENDINTERFACE.
