

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

  ENDMETHOD.

  METHOD constructor.
    super->constructor( iv_title = iv_title ).
    mf_model = abap_true.
    mo_grid_adapter = io_grid_adapter.
  ENDMETHOD.

  METHOD do_before_dynpro_output.
    mo_grid_adapter->set_metadata( ).
  ENDMETHOD.

ENDCLASS.
