"! <p class="shorttext synchronized" lang="en">Events for GUI</p>
INTERFACE zif_uitb_gui_dynpro_events
  PUBLIC .
  "! <p class="shorttext synchronized" lang="en">Event for Screen repainting</p>
  "!
  EVENTS before_output
    EXPORTING
      VALUE(eo_callback) TYPE REF TO zif_uitb_gui_pbo_callback.
  "! <p class="shorttext synchronized" lang="en">Event Entered user command</p>
  EVENTS user_command
    EXPORTING
      VALUE(ev_function_id) TYPE ui_func.
  "! <p class="shorttext synchronized" lang="en">Exit event from Screen</p>
  "!
  EVENTS exit
    EXPORTING
      VALUE(eo_callback) TYPE REF TO zif_uitb_exit_callback.

  "! <p class="shorttext synchronized" lang="en">Screen was exited</p>
  "!
  EVENTS exited.

ENDINTERFACE.
