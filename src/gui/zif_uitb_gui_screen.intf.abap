"! <p class="shorttext synchronized" lang="en">GUI Screen</p>
INTERFACE zif_uitb_gui_screen
  PUBLIC .
*  INTERFACES zif_uitb_gui_events.

  DATA mf_visible TYPE abap_bool read-only.

  "! <p class="shorttext synchronized" lang="en">Displays the screen</p>
  "!
  METHODS show
    IMPORTING
      iv_top    TYPE i OPTIONAL
      iv_left   TYPE i OPTIONAL
      iv_width  TYPE i OPTIONAL
      iv_height TYPE i OPTIONAL.

  "! <p class="shorttext synchronized" lang="en">Leaves the current screen</p>
  "!
  METHODS leave_screen DEFAULT IGNORE
    IMPORTING
      !if_prevent_exit_event TYPE abap_bool DEFAULT abap_true .

ENDINTERFACE.
