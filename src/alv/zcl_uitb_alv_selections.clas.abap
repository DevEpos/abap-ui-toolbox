CLASS ZCL_UITB_alv_selections DEFINITION
  PUBLIC
  INHERITING FROM ZCL_UITB_alv_metadata
  FINAL
  CREATE PUBLIC
  GLOBAL FRIENDS ZCL_UITB_alv_metadata_util
                 ZCL_UITB_alv_grid_adapter.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        ir_controller TYPE REF TO ZIF_UITB_alv_metadata_ctrller.
    METHODS set_mode
      IMPORTING
        value TYPE i DEFAULT zif_uitb_c_alv_selection=>none.
    METHODS get_mode
      RETURNING
        VALUE(result) TYPE i.
    METHODS get_selected_rows
      RETURNING
        VALUE(result) TYPE lvc_t_rows .
    METHODS set_selected_rows
      IMPORTING
        value TYPE lvc_t_rows .
    METHODS get_selected_columns
      RETURNING
        VALUE(result) TYPE lvc_t_fnam.
    METHODS set_selected_columns
      IMPORTING
        !value TYPE salv_t_column .
    METHODS get_current_cell
      RETURNING
        VALUE(result) TYPE ZIF_UITB_alv_types=>ty_cell.
    METHODS set_current_cell
      IMPORTING
        value TYPE ZIF_UITB_alv_types=>ty_cell .
    METHODS set_selected_cells
      IMPORTING
        value TYPE ZIF_UITB_alv_types=>tt_cell.
    METHODS get_selected_cells
      RETURNING
        VALUE(result) TYPE ZIF_UITB_alv_types=>tt_cell.


  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_mode TYPE i VALUE zif_uitb_c_alv_selection=>none.
    DATA mt_selected_rows TYPE lvc_t_rows .
    DATA mt_selected_columns TYPE lvc_t_fnam .
    DATA mt_selected_cells TYPE ZIF_UITB_alv_types=>tt_cell.
    DATA ms_current_cell TYPE ZIF_UITB_alv_types=>ty_cell.
    DATA mf_ccell_received TYPE abap_bool VALUE abap_true .
    DATA mf_cells_received TYPE abap_bool VALUE abap_true .
    DATA mf_cols_received TYPE  abap_bool VALUE abap_true .
    DATA mf_rows_received TYPE  abap_bool VALUE abap_true .

    METHODS get_grid
      RETURNING
        VALUE(result) TYPE REF TO cl_gui_alv_grid.
    METHODS invalidate_selections.
ENDCLASS.



CLASS ZCL_UITB_alv_selections IMPLEMENTATION.
  METHOD set_mode.
    mv_mode = value.

    set_setter_changed( 'SET_MODE' ).

  ENDMETHOD.

  METHOD get_mode.
    result = mv_mode.
  ENDMETHOD.


  METHOD constructor.
    super->constructor(
        ir_controller = ir_controller
        iv_name       = 'SELECTIONS'
    ).
  ENDMETHOD.

  METHOD get_grid.
    DATA(lr_controller) = CAST ZCL_UITB_alv_controller( mr_controller ).
    DATA(lr_adapter) = CAST ZCL_UITB_alv_grid_adapter( lr_controller->mr_adapter ).

    result = lr_adapter->get_grid( ).
  ENDMETHOD.

  METHOD get_selected_rows.
    IF mf_rows_received = abap_false.
      DATA(lr_adapter) = CAST ZCL_UITB_alv_controller( mr_controller )->mr_adapter.

      ZCL_UITB_alv_metadata_util=>get_selected_rows(
          ir_grid       = lr_adapter->get_grid( )
          ir_selections = me
      ).

      mf_rows_received = abap_true.
    ENDIF.

    result = mt_selected_rows.
  ENDMETHOD.

  METHOD set_selected_rows.
    mt_selected_rows = value.

    mf_rows_received = abap_true.

    CHECK mr_controller IS BOUND.

    mr_controller->set_changed(
        iv_name         = mv_name
        iv_flavour      = zif_uitb_c_alv_chglist_flavor=>selections
        iv_method       = 'SET_SELECTED_ROWS'
        iv_object       = zif_uitb_c_alv_selection_type=>rows
        ir_ref          = me
    ).
  ENDMETHOD.

  METHOD get_selected_columns.
    IF mf_cols_received = abap_false.
      DATA(lr_adapter) = CAST ZCL_UITB_alv_controller( mr_controller )->mr_adapter.

      ZCL_UITB_alv_metadata_util=>get_selected_columns(
          ir_grid       = lr_adapter->get_grid( )
          ir_selections = me
      ).

      mf_cols_received = abap_true.
    ENDIF.

    result = mt_selected_columns.
  ENDMETHOD.

  METHOD set_selected_columns.
    mt_selected_columns = value.

    mf_cols_received = abap_true.

    CHECK mr_controller IS BOUND.

    mr_controller->set_changed(
        iv_name         = mv_name
        iv_flavour      = zif_uitb_c_alv_chglist_flavor=>selections
        iv_method       = 'SET_SELECTED_COLUMNS'
        iv_object       = zif_uitb_c_alv_selection_type=>columns
        ir_ref          = me
    ).
  ENDMETHOD.

  METHOD get_current_cell.
    IF mf_ccell_received = abap_false.
      DATA(lr_adapter) = CAST ZCL_UITB_alv_controller( mr_controller )->mr_adapter.

      ZCL_UITB_alv_metadata_util=>get_current_cell(
          ir_grid       = lr_adapter->get_grid( )
          ir_selections = me
      ).

      mf_ccell_received = abap_true.
    ENDIF.

    result = ms_current_cell.
  ENDMETHOD.

  METHOD set_current_cell.
    ms_current_cell = value.

    mf_ccell_received = abap_true.

    CHECK mr_controller IS BOUND.

    mr_controller->set_changed(
        iv_name         = mv_name
        iv_flavour      = zif_uitb_c_alv_chglist_flavor=>selections
        iv_method       = 'SET_CURRENT_CELL'
        iv_object       = zif_uitb_c_alv_selection_type=>cell
        ir_ref          = me
    ).
  ENDMETHOD.

  METHOD get_selected_cells.
    IF mf_cells_received = abap_false.
      DATA(lr_adapter) = CAST ZCL_UITB_alv_controller( mr_controller )->mr_adapter.

      ZCL_UITB_alv_metadata_util=>get_selected_cells(
          ir_grid       = lr_adapter->get_grid( )
          ir_selections = me
      ).

      mf_cells_received = abap_true.
    ENDIF.

    result = mt_selected_cells.
  ENDMETHOD.

  METHOD set_selected_cells.

    mt_selected_cells = value.
    mf_cells_received = abap_true.

    CHECK mr_controller IS BOUND.

    mr_controller->set_changed(
        iv_name         = mv_name
        iv_flavour      = zif_uitb_c_alv_chglist_flavor=>selections
        iv_method       = 'SET_SELECTED_CELLS'
        iv_object       = zif_uitb_c_alv_selection_type=>cells
        ir_ref          = me
    ).
  ENDMETHOD.

  METHOD invalidate_selections.
    CLEAR: mf_ccell_received,
           mf_cells_received,
           mf_rows_received,
           mf_cols_received.
  ENDMETHOD.

ENDCLASS.
