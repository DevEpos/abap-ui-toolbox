INTERFACE ZIF_UITB_view_callback
  PUBLIC .
  EVENTS before_output
    EXPORTING
      VALUE(er_callback) TYPE REF TO ZIF_UITB_pbo_callback.
  EVENTS user_command
    EXPORTING
      VALUE(ev_function_id) TYPE sy-ucomm
      VALUE(er_callback) TYPE REF TO ZIF_UITB_pai_callback.
  EVENTS exit
    EXPORTING
      VALUE(er_callback) TYPE REF TO ZIF_UITB_exit_callback.
ENDINTERFACE.
