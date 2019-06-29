

CLASS cl_dialog IMPLEMENTATION.

  METHOD create_content.
    mo_gui_alv_grid = NEW cl_gui_alv_grid(
        i_parent        = io_container
        i_lifetime      = mv_lifetime
        i_fcat_complete = abap_true
        i_appl_events   = abap_true
    ).
  ENDMETHOD.

  METHOD zif_uitb_gui_command_handler~execute_command.
    DATA(lv_function) = io_command->mv_function.
    mo_gui_alv_grid->set_function_code( CHANGING c_ucomm = lv_function ).
  ENDMETHOD.

  METHOD constructor.

    super->constructor( iv_title = iv_title ).
    mo_grid_adapter = io_grid_adapter.
  ENDMETHOD.

ENDCLASS.

CLASS cl_modal_dialog IMPLEMENTATION.

  METHOD create_content.
    mo_gui_alv_grid = NEW cl_gui_alv_grid(
        i_parent        = io_container
        i_lifetime      = mv_lifetime
        i_fcat_complete = abap_true
        i_appl_events   = abap_true
    ).
  ENDMETHOD.

  METHOD zif_uitb_gui_command_handler~execute_command.
    DATA(lv_function) = io_command->mv_function.
    cl_gui_alv_grid=>transfer_fcode_lvc_to_slis(
      EXPORTING
        i_fcode_lvc    = io_command->mv_function
      IMPORTING
        e_fcode_slis   = DATA(lv_slis_code)
      EXCEPTIONS
        no_match_found = 1
        OTHERS         = 2
    ).
    IF sy-subrc = 0.
      mo_gui_alv_grid->set_function_code( CHANGING c_ucomm = lv_slis_code ).
    ENDIF.

  ENDMETHOD.

  METHOD constructor.
    super->constructor( iv_title = iv_title ).
    mf_model = abap_true.
    mo_grid_adapter = io_grid_adapter.
  ENDMETHOD.

  METHOD do_before_dynpro_output.
    IF io_callback->is_first_screen_call( ).
      mo_grid_adapter->set_metadata( ).
    ENDIF.

    io_callback->map_fkey_functions( VALUE #(
      ( fkey = zif_uitb_c_gui_screen=>c_functions-search      mapped_function = zif_uitb_c_alv_functions=>find )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-search_more mapped_function = zif_uitb_c_alv_functions=>find_more )
    ) ).
  ENDMETHOD.

ENDCLASS.
