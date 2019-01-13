CLASS lcl_exit_callback IMPLEMENTATION.

  METHOD zif_uitb_exit_callback~cancel_exit.
    mf_exit_cancelled = abap_true.
  ENDMETHOD.

  METHOD zif_uitb_exit_callback~exit_cancelled.
    rf_exit_cancelled = mf_exit_cancelled.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_pai_callback IMPLEMENTATION.

  METHOD zif_uitb_pai_callback~exit_screen.
    lcl_local_controller=>clean_up( ).
    lcl_local_controller=>leave_screen( ).
  ENDMETHOD.

ENDCLASS.


CLASS lcl_pbo_callback IMPLEMENTATION.

  METHOD zif_uitb_pbo_callback~is_first_screen_call.
    rf_is_first = mf_is_first_screen_call.
    IF mf_is_first_screen_call = abap_true.
      CLEAR mf_is_first_screen_call.
    ENDIF.
  ENDMETHOD.

  METHOD zif_uitb_pbo_callback~deactivate_function.
    APPEND iv_function TO mt_function_exclude.
  ENDMETHOD.

  METHOD zif_uitb_pbo_callback~set_title.
    gs_view_data-title = iv_title.
  ENDMETHOD.

  METHOD get_exclude.
    et_exclude_tab = mt_function_exclude.
  ENDMETHOD.

  METHOD constructor.
    mf_is_first_screen_call = if_first_screen_call.
  ENDMETHOD.

  METHOD zif_uitb_pbo_callback~exit_screen.
    lcl_local_controller=>clean_up( ).
    lcl_local_controller=>leave_screen( ).
  ENDMETHOD.

  METHOD zif_uitb_pbo_callback~set_status.
    gs_view_data-status = iv_status.
    gs_view_data-status_prog = COND #(
        WHEN iv_progname IS INITIAL THEN sy-repid
        ELSE iv_progname
    ).
  ENDMETHOD.

  METHOD zif_uitb_pbo_callback~deactivate_functions.
    mt_function_exclude = VALUE #(
      BASE mt_function_exclude
      ( LINES OF it_functions )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS lcl_local_controller IMPLEMENTATION.

  METHOD pai.
    DATA(lv_function) = ok_code.
    CLEAR ok_code.

    CHECK lv_function IS NOT INITIAL.

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
        ir_callback = NEW lcl_pai_callback( )
      ).
    ENDIF.
  ENDMETHOD.

  METHOD pbo.
    DATA: lt_exclude_function TYPE STANDARD TABLE OF sy-ucomm.

    IF gs_view_data-is_first_call = abap_true.
      clear_functions( ).
    ENDIF.


    CLEAR: gs_view_data-status.

    DATA(lr_callback) = NEW lcl_pbo_callback( gs_view_data-is_first_call ).
    gs_view_data-controller->raise_before_output( lr_callback ).

    SET TITLEBAR '0100' WITH gs_view_data-title.

    " fill dynamic function
    fill_dynamic_functions( ).

    lr_callback->get_exclude( IMPORTING et_exclude_tab = lt_exclude_function ).
    IF gs_view_data-status IS NOT INITIAL.
      SET PF-STATUS gs_view_data-status OF PROGRAM gs_view_data-status_prog EXCLUDING lt_exclude_function.
    ELSE.
      IF gs_view_data-as_dialog = abap_true.
        SET PF-STATUS '0101' EXCLUDING lt_exclude_function.
      ELSE.
        SET PF-STATUS '0100' EXCLUDING lt_exclude_function.
      ENDIF.
    ENDIF.
    CLEAR gs_view_data-is_first_call.
  ENDMETHOD.

  METHOD exit.
    DATA(lf_exit) = abap_true.

*... Check if exit event should be called
    IF ok_code = zif_uitb_template_prog=>c_func_leave OR
       ok_code = zif_uitb_template_prog=>c_func_quit OR
       ok_code = zif_uitb_template_prog=>c_func_cancel.
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
    ASSIGN ('CHOOSE') TO FIELD-SYMBOL(<lv_choose_function>).
    IF sy-subrc = 0.
      CLEAR <lv_choose_function>.
    ENDIF.

    DO gc_max_function_number TIMES.
      DATA(lv_dyn_func_name) = 'FUNC' && sy-index.
      ASSIGN (lv_dyn_func_name) TO FIELD-SYMBOL(<ls_dyn_function>).
      IF sy-subrc = 0.
        CLEAR <ls_dyn_function>.
      ENDIF.
    ENDDO.
  ENDMETHOD.

  METHOD leave_screen.
    SET SCREEN 0.
    LEAVE SCREEN.
  ENDMETHOD.

  METHOD fill_dynamic_functions.
    LOOP AT gs_view_data-functions ASSIGNING FIELD-SYMBOL(<ls_dynamic_function>).
      ASSIGN (<ls_dynamic_function>-function_id) TO FIELD-SYMBOL(<ls_dyn_function_dynpro>).
      IF sy-subrc = 0.
        <ls_dyn_function_dynpro> = <ls_dynamic_function>-function_info.
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
