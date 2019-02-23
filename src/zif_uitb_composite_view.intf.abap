INTERFACE zif_uitb_composite_view
  PUBLIC .

  INTERFACES zif_uitb_view.

  "! <p class="shorttext synchronized" lang="en">Set Child visible/hidden</p>
  "!
  METHODS set_child_visibility
    IMPORTING
      io_child   TYPE REF TO zif_uitb_view
      if_visible TYPE abap_bool DEFAULT abap_true.
ENDINTERFACE.
