"! <p class="shorttext synchronized" lang="en">Screen Controller</p>
INTERFACE zif_uitb_screen_controller
  PUBLIC .


  DATA mf_new_sequence TYPE abap_bool .
  DATA mf_first_call TYPE abap_bool .

  TYPES:
    "! Defines the Size of a screen
    BEGIN OF ty_s_screen_size,
      top     TYPE i,
      left    TYPE i,
      columns TYPE i,
      rows    TYPE i,
    END OF ty_s_screen_size.

  EVENTS leave .

  "! <p class="shorttext synchronized" lang="en">After Input Handler</p>
  METHODS handle_user_command
    CHANGING
      !cv_function_code TYPE sy-ucomm .
  "! <p class="shorttext synchronized" lang="en">Loads Context Menu</p>
  METHODS load_context_menu DEFAULT IGNORE
    IMPORTING
      !ir_menu             TYPE REF TO cl_ctmenu
      !iv_screen_parameter TYPE string OPTIONAL .
  "! <p class="shorttext synchronized" lang="en">Determination of the current cursor position</p>
  METHODS determine_cursor DEFAULT IGNORE .
  "! <p class="shorttext synchronized" lang="en">Before Output Handler</p>
  METHODS pbo .
  "! <p class="shorttext synchronized" lang="en">Set the GUI Status for the screen</p>
  METHODS set_status .
  "! <p class="shorttext synchronized" lang="en">Call the Screen</p>
  METHODS call_screen
    IMPORTING
      !if_as_dialog  TYPE abap_bool OPTIONAL
      is_screen_size TYPE ty_s_screen_size OPTIONAL.
  "! <p class="shorttext synchronized" lang="en">Free allocated Resourcen</p>
  METHODS free_screen_resources DEFAULT IGNORE .
  "! <p class="shorttext synchronized" lang="en">Cancelled by User</p>
  METHODS cancel DEFAULT IGNORE
    IMPORTING
      VALUE(iv_function_code) TYPE sy-ucomm OPTIONAL .
  METHODS was_not_cancelled DEFAULT IGNORE
    RETURNING
      VALUE(rf_not_cancelled) TYPE boolean .
  "! <p class="shorttext synchronized" lang="en">Get the ID of the report of the screen</p>
  METHODS get_report_id DEFAULT IGNORE
    RETURNING
      VALUE(result) TYPE repid .
  "! <p class="shorttext synchronized" lang="en">Get the ID of the screen</p>
  METHODS get_screen_id DEFAULT IGNORE
    RETURNING
      VALUE(result) TYPE dynnr .
ENDINTERFACE.
