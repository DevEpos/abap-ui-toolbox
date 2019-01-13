class ZCL_UITB_ALV definition
  public
  final
  create private

  global friends ZCL_UITB_ALV_GRID_ADAPTER
                 ZCL_UITB_ALV_METADATA_UTIL .

public section.

  interfaces ZIF_UITB_GUI_CONTROL .

  class-methods CREATE_ALV
    importing
      !IR_DATA type ref to DATA
      !IV_DESCRIPTION_LANGUAGE type LANGU default SY-LANGU
      !IR_CONTAINER type ref to CL_GUI_CONTAINER
      !IF_EDITABLE type ABAP_BOOL optional
    returning
      value(RR_ALV) type ref to ZCL_UITB_ALV .
  methods GET_DATA
    returning
      value(RR_DATA) type ref to DATA .
  methods SET_FUNCTION
    importing
      !IV_FUNCTION type UI_FUNC .
  methods GET_METADATA .
  methods GET_FILTERS
    returning
      value(RESULT) type ref to ZCL_UITB_ALV_FILTERS .
  methods SET_SORTING
    importing
      !IT_SORTING type LVC_T_SORT .
  methods SET_EDITABLE
    importing
      !VALUE type ABAP_BOOL .
  methods DISPLAY
    raising
      ZCX_UITB_ALV_ERROR .
  methods GET_COLUMNS
    returning
      value(RESULT) type ref to ZCL_UITB_ALV_COLUMNS .
  methods REGISTER_EDIT_FOR_MODIFIED
    importing
      !VALUE type ABAP_BOOL default ABAP_TRUE .
  methods REFRESH
    importing
      !IF_SOFT type ABAP_BOOL optional
      !IS_STABLE type LVC_S_STBL optional
      !IF_KEEP_SCROLL_POSITION type ABAP_BOOL optional .
  methods GET_SORTING
    returning
      value(RESULT) type ref to ZCL_UITB_ALV_SORTS .
  methods GET_LAYOUT
    returning
      value(RESULT) type ref to ZCL_UITB_ALV_LAYOUT .
  methods GET_EVENTS
    returning
      value(RESULT) type ref to ZCL_UITB_ALV_EVENTS .
  methods GET_FUNCTIONS
    returning
      value(RESULT) type ref to ZCL_UITB_ALV_FUNCTIONS .
  methods GET_SELECTIONS
    returning
      value(RESULT) type ref to ZCL_UITB_ALV_SELECTIONS .
  methods GET_DATA_CHANGES
    returning
      value(RESULT) type ref to ZCL_UITB_ALV_DATA .
  methods GET_FUNCTIONAL_SETTINGS
    returning
      value(RESULT) type ref to ZCL_UITB_ALV_FUNC_SETTINGS .
  methods GET_DISPLAY_SETTINGS
    returning
      value(RESULT) type ref to ZCL_UITB_ALV_DISPLAY_SETTINGS .
  methods SET_DATA
    importing
      !IR_DATA type ref to DATA .
protected section.

  methods PERFORM_QUICK_FILTER
    importing
      !IF_EXCLUDE type ABAP_BOOL optional .
private section.

  constants:
    BEGIN OF c_change_character,
        sorting TYPE char1 VALUE 'S',
        layout  TYPE char1 VALUE 'L',
        columns TYPE char1 VALUE 'C',
      END OF c_change_character .
  constants C_DELETE_ROW_CUSTOM type STRING value 'DELETE_ROW' ##NO_TEXT.
  data:
    mt_changes TYPE HASHED TABLE OF char1 WITH UNIQUE KEY table_line .
  data MR_CONTROLLER type ref to ZCL_UITB_ALV_CONTROLLER .
  data MR_ALV_DATA_WRAPPER type ref to ZCL_UITB_ALV_DATA .
  data MR_FUNCTIONS type ref to ZCL_UITB_ALV_FUNCTIONS .
  data MR_FUNCTIONAL_SETTINGS type ref to ZCL_UITB_ALV_FUNC_SETTINGS .
  data MR_FILTERS type ref to ZCL_UITB_ALV_FILTERS .
  data MR_DISPLAY_SETTINGS type ref to ZCL_UITB_ALV_DISPLAY_SETTINGS .
  data MR_DATA type ref to DATA .
  data MR_COLUMNS type ref to ZCL_UITB_ALV_COLUMNS .
  data MR_EVENTS type ref to ZCL_UITB_ALV_EVENTS .
  data MT_EXCLUDE_TOOLBAR type UI_FUNCTIONS .
  data MF_REGISTER_MODIFIED type ABAP_BOOL .
  data MR_SELECTIONS type ref to ZCL_UITB_ALV_SELECTIONS .
  data MT_SORTING type LVC_T_SORT .
  data MR_CONTAINER type ref to CL_GUI_CONTAINER .
  data MR_SORTING type ref to ZCL_UITB_ALV_SORTS .
  data MR_LAYOUT type ref to ZCL_UITB_ALV_LAYOUT .

  methods INIT
    importing
      !IF_EDITABLE type ABAP_BOOL
      !IV_DESCRIPTION_LANGUAGE type LANGU .
  methods CONSTRUCTOR
    importing
      !IR_DATA type ref to DATA
      !IR_CONTAINER type ref to CL_GUI_CONTAINER .
ENDCLASS.



CLASS ZCL_UITB_ALV IMPLEMENTATION.


  METHOD constructor.
    mr_data = ir_data.
    mr_container = ir_container.
  ENDMETHOD.


  METHOD create_alv.
    rr_alv = NEW #(
      ir_data      = ir_data
      ir_container = ir_container
    ).

    rr_alv->init(
        if_editable             = if_editable
        iv_description_language = iv_description_language
    ).
  ENDMETHOD.


  METHOD display.
    CHECK mr_controller IS BOUND.

    mr_controller->display( ).

  ENDMETHOD.


  METHOD get_columns.
    IF mr_columns IS INITIAL.
      mr_columns = NEW #(
        ir_data       = mr_data
        ir_controller = mr_controller
      ).
    ENDIF.

    result = mr_columns.
  ENDMETHOD.


  METHOD get_data.
    rr_data = mr_data.
  ENDMETHOD.


  METHOD get_data_changes.
    IF mr_alv_data_wrapper IS NOT BOUND.
      mr_alv_data_wrapper = NEW #(
        ir_controller = mr_controller
        ir_data       = mr_data
      ).
    ENDIF.

    result = mr_alv_data_wrapper.
  ENDMETHOD.


  METHOD get_display_settings.
    IF mr_display_settings IS NOT BOUND.
      mr_display_settings = NEW #( ir_controller = mr_controller ).
    ENDIF.

    result = mr_display_settings.
  ENDMETHOD.


  METHOD get_events.
    IF mr_events IS NOT BOUND.
      mr_events = NEW #( ir_controller = mr_controller ).
    ENDIF.

    result = mr_events.
  ENDMETHOD.


  METHOD get_filters.
    IF mr_filters IS INITIAL.
      mr_filters = NEW zcl_uitb_alv_filters(
        ir_controller = mr_controller
        ir_columns    = get_columns( )
      ).
    ENDIF.

    result = mr_filters.
  ENDMETHOD.


  METHOD get_functional_settings.
    IF mr_functional_settings IS NOT BOUND.
      mr_functional_settings = NEW #( ir_controller = mr_controller ).
    ENDIF.

    result = mr_functional_settings.
  ENDMETHOD.


  METHOD get_functions.
    IF mr_functions IS INITIAL.
      mr_functions = NEW #( mr_controller ).
    ENDIF.

    result = mr_functions.
  ENDMETHOD.


  METHOD get_layout.
    IF mr_layout IS INITIAL.
      mr_layout = NEW #( ir_controller = mr_controller ).
    ENDIF.

    result = mr_layout.
  ENDMETHOD.


  METHOD get_metadata.
    DATA(lr_adapter) = mr_controller->mr_adapter.
    IF lr_adapter IS BOUND.
      lr_adapter->get_metadata( ).
    ENDIF.
  ENDMETHOD.


  METHOD get_selections.
    IF mr_selections IS INITIAL.
      mr_selections = NEW #( ir_controller = mr_controller ).
    ENDIF.

    result = mr_selections.
  ENDMETHOD.


  METHOD get_sorting.
    IF mr_sorting IS INITIAL.
      mr_sorting = NEW #( ir_controller = mr_controller
                          ir_columns    = get_columns( ) ).
    ENDIF.

    result = mr_sorting.
  ENDMETHOD.


  METHOD init.

    " create the controller
    mr_controller = NEW #( ir_model = me ).

    get_columns( )->set_description_language( iv_description_language ).
    get_functional_settings( ).
    get_events( ).
    get_display_settings( ).
    get_functions( ).
    get_data_changes( ).

    mr_display_settings->set_editable( if_editable ).

    " create columns from input data
    set_data( mr_data ).
  ENDMETHOD.


  METHOD perform_quick_filter.
    TYPES:
      BEGIN OF lty_filter,
        column TYPE fieldname,
        selopt TYPE zuitb_generic_range_itab,
      END OF lty_filter.

    TYPES: ltt_filter TYPE SORTED TABLE OF lty_filter WITH UNIQUE KEY column.

    DATA: lr_filter TYPE REF TO zcl_uitb_alv_filter.

    FIELD-SYMBOLS: <lt_table> TYPE table.

    get_metadata( ).
    DATA(lr_filters) = get_filters( ).
    DATA(lr_columns) = get_columns( ).
    DATA(lt_selected_cells) = get_selections( )->get_selected_cells( ).

    SORT lt_selected_cells BY row.

    ASSIGN mr_data->* TO <lt_table>.

    LOOP AT lt_selected_cells ASSIGNING FIELD-SYMBOL(<ls_cell>).

      AT NEW row.
        ASSIGN <lt_table>[ <ls_cell>-row ] TO FIELD-SYMBOL(<ls_line>).
        CHECK sy-subrc = 0.
      ENDAT.

      TRY.
          DATA(lr_column) = lr_columns->get_column( <ls_cell>-column ).
        CATCH zcx_uitb_alv_not_found.
          CONTINUE.
      ENDTRY.

      ASSIGN COMPONENT <ls_cell>-column OF STRUCTURE <ls_line> TO FIELD-SYMBOL(<lv_value>).
      TRY.
          lr_filter = lr_filters->get_filter( <ls_cell>-column ).
        CATCH zcx_uitb_alv_not_found.
          lr_filter = lr_filters->add_filter( iv_columnname = <ls_cell>-column ).
      ENDTRY.

      CHECK lr_filter IS BOUND.

      lr_filter->add_selopt(
          iv_sign   = COND #( WHEN if_exclude = abap_true THEN 'E' ELSE 'I' )
          iv_option = 'EQ'
          iv_low    = |{ <lv_value> }|
      ).
    ENDLOOP.

    IF sy-subrc = 0.
      refresh(
          is_stable = VALUE #( row = abap_true col = abap_true )
      ).
    ENDIF.
  ENDMETHOD.


  METHOD refresh.
    CHECK mr_controller IS BOUND.

    DATA(lv_refresh_mode) = COND #( WHEN if_soft = abap_true THEN
                                      zif_uitb_c_alv_refresh=>soft
                                    ELSE
                                      zif_uitb_c_alv_refresh=>full ).

    mr_controller->zif_uitb_alv_metadata_ctrller~set_changed(
        iv_name         = 'ZCL_UITB_EDITABLE_ALV'
        iv_flavour      = zif_uitb_c_alv_chglist_flavor=>refresh
        iv_refresh_mode = lv_refresh_mode
        iv_method       = 'REFRESH'
    ).

    IF is_stable IS SUPPLIED.
      mr_controller->refresh(
          is_stable               = is_stable
          if_keep_scroll_position = if_keep_scroll_position
      ).
    ELSE.
      mr_controller->refresh(
          is_stable               = VALUE #( row = abap_true col = abap_true )
          if_keep_scroll_position = if_keep_scroll_position
      ).
    ENDIF.
  ENDMETHOD.


  METHOD register_edit_for_modified.
    mf_register_modified = value.
  ENDMETHOD.


  METHOD set_data.

*.. @TODO: Check if new data is allowed
    check ir_data is not INITIAL.

    DATA(lr_columns) = get_columns( ).
    mr_data = ir_data.

    zcl_uitb_alv_data_descr=>describe_table(
        ir_columns = lr_columns
        ir_table   = mr_data
    ).
    get_filters( )->clear( ).

    CHECK mr_controller IS BOUND.

    mr_controller->zif_uitb_alv_metadata_ctrller~set_changed(
        iv_name         = 'ZCL_UITB_EDITABLE_ALV'
        iv_flavour      = zif_uitb_c_alv_chglist_flavor=>data_set
        iv_method       = 'SET_DATA'
    ).

  ENDMETHOD.


  METHOD set_editable.
    " @TODO: maybe this is needed if grid can be switched into read only mode
  ENDMETHOD.


  METHOD set_function.
    CHECK mr_controller IS BOUND.

    mr_controller->set_function( iv_function ).
  ENDMETHOD.


  METHOD set_sorting.
    mt_sorting = it_sorting.
    INSERT c_change_character-sorting INTO TABLE mt_changes.
  ENDMETHOD.


  METHOD zif_uitb_gui_control~focus.
    CHECK mr_controller IS BOUND.
    mr_controller->focus( ).
  ENDMETHOD.
ENDCLASS.
