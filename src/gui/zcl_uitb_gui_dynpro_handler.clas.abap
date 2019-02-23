CLASS zcl_uitb_gui_dynpro_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_uitb_gui_dynpro_events.

    "! <p class="shorttext synchronized" lang="en">Frees allocated resources</p>
    "!
    METHODS free_resources .
    "! <p class="shorttext synchronized" lang="en">Raises the PBO event</p>
    "!
    METHODS raise_before_output
      IMPORTING
        !io_callback TYPE REF TO zif_uitb_gui_pbo_callback .
    "! <p class="shorttext synchronized" lang="en">Raises the exit event</p>
    "!
    METHODS raise_exit
      IMPORTING
        !io_callback TYPE REF TO zif_uitb_exit_callback .
    "! <p class="shorttext synchronized" lang="en">Raises the PAI event</p>
    "!
    "! This is used to pass classic user commands from the Screen
    METHODS raise_user_command
      IMPORTING
        !iv_function TYPE sy-ucomm.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_uitb_gui_dynpro_handler IMPLEMENTATION.

  METHOD free_resources.
    RAISE EVENT zif_uitb_gui_dynpro_events~exited.
  ENDMETHOD.


  METHOD raise_before_output.
    RAISE EVENT zif_uitb_gui_dynpro_events~before_output
      EXPORTING
        eo_callback = io_callback.
  ENDMETHOD..


  METHOD raise_exit.
    RAISE EVENT zif_uitb_gui_dynpro_events~exit
      EXPORTING
        eo_callback = io_callback.
  ENDMETHOD.


  METHOD raise_user_command.
    RAISE EVENT zif_uitb_gui_dynpro_events~user_command
      EXPORTING
        ev_function_id = iv_function.
  ENDMETHOD.
ENDCLASS.
