MODULE pai INPUT.
  zcl_uitb_screen_util=>handle_gui_command( CHANGING cv_ok_code = ok_code ).
  lcl_local_controller=>pai( ).
ENDMODULE.

MODULE exit INPUT.
  lcl_local_controller=>exit( ).
ENDMODULE.
