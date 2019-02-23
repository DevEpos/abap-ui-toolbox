CLASS lcl_exit_callback IMPLEMENTATION.

  METHOD zif_uitb_exit_callback~cancel_exit.
    mf_exit_cancelled = abap_true.
  ENDMETHOD.

  METHOD zif_uitb_exit_callback~exit_cancelled.
    rf_exit_cancelled = mf_exit_cancelled.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_pbo_callback IMPLEMENTATION.

  METHOD zif_uitb_gui_pbo_callback~is_first_screen_call.
    rf_is_first = mf_is_first_screen_call.
    IF mf_is_first_screen_call = abap_true.
      CLEAR mf_is_first_screen_call.
    ENDIF.
  ENDMETHOD.

  METHOD zif_uitb_gui_pbo_callback~deactivate_function.
    APPEND iv_function TO mt_function_exclude.
  ENDMETHOD.

  METHOD zif_uitb_gui_pbo_callback~set_title.
    gs_view_data-title = iv_title.
  ENDMETHOD.

  METHOD constructor.
    mf_is_first_screen_call = if_first_screen_call.
  ENDMETHOD.

  METHOD zif_uitb_gui_pbo_callback~set_status.
    gs_view_data-status = iv_status.
    gs_view_data-status_prog = COND #(
        WHEN iv_progname IS INITIAL THEN sy-repid
        ELSE iv_progname
    ).
  ENDMETHOD.

  METHOD zif_uitb_gui_pbo_callback~deactivate_functions.
    mt_function_exclude = VALUE #(
      BASE mt_function_exclude
      ( LINES OF it_functions )
    ).
  ENDMETHOD.

  METHOD zif_uitb_gui_pbo_callback~map_fkey_function.
    CHECK NOT line_exists( mt_fkey_map[ fkey = iv_fkey ] ).

    mt_fkey_map = VALUE #(
      BASE mt_fkey_map
      ( fkey            = iv_fkey
        mapped_function = iv_mapped_function
        text            = iv_text )
    ).
  ENDMETHOD.

  METHOD zif_uitb_gui_pbo_callback~map_fkey_functions.
    mt_fkey_map = it_fkey_map.
  ENDMETHOD.

ENDCLASS.



CLASS lcl_local_controller IMPLEMENTATION.

  METHOD pai.
    DATA(lv_function) = ok_code.
    CLEAR ok_code.

    CHECK lv_function IS NOT INITIAL.

    IF lv_function CP '%_GC*'.
      cl_gui_cfw=>dispatch( ).
      CLEAR lv_function.
    ENDIF.

    IF lv_function = zif_uitb_c_gui_screen=>c_shortcuts_function AND
       gs_view_data-fkey_map IS NOT INITIAL.
*.... Display dialog with all registered F-Key in this screen
      zcl_uitb_app_shortcuts_viewer=>display_shortcuts( gs_view_data-fkey_map ).
      RETURN.
    ENDIF.

*.. Map fkey to user function, if possible
    lv_function = VALUE #( gs_view_data-fkey_map[ fkey = lv_function ]-mapped_function DEFAULT lv_function ).

*.. Check for special leave function where no exit event should be sent
    IF lv_function = zif_uitb_template_prog=>c_func_leave_no_exit_event.
      clean_up( ).
      leave_screen( ).
*.... handle the function in the callback program
    ELSEIF lv_function = zif_uitb_template_prog=>c_func_leave.
      DATA(lf_exit) = abap_true.
      should_exit( CHANGING cf_exit = lf_exit ).
      IF lf_exit = abap_true.
        clean_up( ).
        leave_screen( ).
      ELSE.
        CLEAR ok_code.
      ENDIF.
    ELSE.
      gs_view_data-controller->raise_user_command(
        iv_function = lv_function
      ).
    ENDIF.
  ENDMETHOD.

  METHOD pbo.
    DATA(lo_callback) = NEW lcl_pbo_callback( gs_view_data-is_first_call ).
    gs_view_data-controller->raise_before_output( lo_callback ).

    SET TITLEBAR '0100' WITH gs_view_data-title.

    clear_functions( ).
    gs_view_data-fkey_map = lo_callback->mt_fkey_map.

    fill_dynamic_functions( ).

    DATA(lt_excluding) = lo_callback->mt_function_exclude.

*.. Hide shortcut function if no function is registered
    IF lo_callback->mt_fkey_map IS INITIAL.
      lt_excluding = VALUE #( BASE lt_excluding ( zif_uitb_c_gui_screen=>c_shortcuts_function ) ).
    ENDIF.

    IF gs_view_data-status IS NOT INITIAL.
      SET PF-STATUS gs_view_data-status OF PROGRAM gs_view_data-status_prog EXCLUDING lt_excluding.
    ELSE.
      IF gs_view_data-as_dialog = abap_true.
        SET PF-STATUS '0101' EXCLUDING lt_excluding.
      ELSE.
        SET PF-STATUS '0100' EXCLUDING lt_excluding.
      ENDIF.
    ENDIF.
    CLEAR gs_view_data-is_first_call.
  ENDMETHOD.

  METHOD exit.
    DATA(lf_exit) = abap_true.

*... Check if exit event should be called
    IF ok_code = zif_uitb_c_gui_screen=>c_functions-leave OR
       ok_code = zif_uitb_c_gui_screen=>c_functions-quit OR
       ok_code = zif_uitb_c_gui_screen=>c_functions-cancel.
      should_exit( CHANGING cf_exit = lf_exit ).
      IF lf_exit = abap_false.
        CLEAR ok_code.
        RETURN.
      ENDIF.
    ENDIF.

    IF ok_code = zif_uitb_template_prog=>c_func_cancel.
      CLEAR ok_code.
      clean_up( ).
      leave_screen( ).
    ELSEIF ok_code = zif_uitb_template_prog=>c_func_quit.
      CLEAR ok_code.
      clean_up( ).
      LEAVE PROGRAM.
    ENDIF.
  ENDMETHOD.

  METHOD clear_functions.
    CLEAR gs_function.
  ENDMETHOD.

  METHOD leave_screen.
    SET SCREEN 0.
    LEAVE SCREEN.
  ENDMETHOD.

  METHOD fill_dynamic_functions.
    FIELD-SYMBOLS: <ls_dyn_function> TYPE smp_dyntxt.

    LOOP AT gs_view_data-fkey_map ASSIGNING FIELD-SYMBOL(<ls_fkey_map>).
      ASSIGN COMPONENT <ls_fkey_map>-fkey OF STRUCTURE gs_function TO <ls_dyn_function>.
      IF sy-subrc = 0.
        <ls_dyn_function>-text = <ls_fkey_map>-text.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD should_exit.
    DATA(lr_exit_callback) = CAST zif_uitb_exit_callback( NEW lcl_exit_callback( ) ).
    gs_view_data-controller->raise_exit( lr_exit_callback ).

    cf_exit = xsdbool( NOT lr_exit_callback->exit_cancelled( ) ).
  ENDMETHOD.

  METHOD clean_up.
    gs_view_data-controller->free_resources( ).
    CLEAR gs_view_data.

    " reset to last last screen controller
    IF gt_view_controller IS NOT INITIAL.
      gs_view_data = gt_view_controller[ 1 ].
      DELETE gt_view_controller INDEX 1.

      clear_functions( ).
      fill_dynamic_functions( ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
