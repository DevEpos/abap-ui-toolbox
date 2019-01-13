class ZCL_UITB_ALV_EVENTS definition
  public
  inheriting from ZCL_UITB_ALV_METADATA
  final
  create public

  global friends ZCL_UITB_ALV_EVT_CONTROLLER .

public section.

  events DATA_CHANGED
    exporting
      value(ER_CHANGE_PROTOCOL) type ref to CL_ALV_CHANGED_DATA_PROTOCOL optional
      value(EF_ONF4) type ABAP_BOOL optional
      value(EF_ONF4_BEFORE) type ABAP_BOOL optional
      value(EF_ONF4_AFTER) type ABAP_BOOL optional
      value(EV_FUNCTION) type SY-UCOMM optional .
  events F4
    exporting
      value(EV_FIELDNAME) type LVC_FNAME optional
      value(EV_FIELDVALUE) type LVC_VALUE optional
      value(ES_ROW_NO) type LVC_S_ROID optional
      value(ER_EVENT_DATA) type ref to CL_ALV_EVENT_DATA optional
      value(ET_BAD_CELLS) type LVC_T_MODI optional
      value(EF_DISPLAY) type ABAP_BOOL optional .
  events BEFORE_FUNCTION
    exporting
      value(EV_FUNCTION) type UI_FUNC optional .
  events AFTER_FUNCTION
    exporting
      value(EV_FUNCTION) type UI_FUNC optional .
    "! Event for chosen ALV function e.g. toolbar button clicked
    "!
    "! @parameter ev_function | the function code
    "! @parameter ev_tag      | the tag of the function
  events FUNCTION_CHOSEN
    exporting
      value(EV_FUNCTION) type UI_FUNC optional
      value(EV_TAG) type STRING optional .
  events DOUBLE_CLICK
    exporting
      value(EV_ROW) type LVC_ROID
      value(EV_COLUMN) type LVC_FNAME .
  events LINK_CLICK
    exporting
      value(EV_ROW) type I
      value(EV_COLUMN) type LVC_FNAME .
  events DROP_GET_FLAVOR
    exporting
      value(EV_ROW) type I
      value(EV_COLUMN) type LVC_FNAME
      value(ES_ROW_NO) type LVC_S_ROID
      value(ER_DRAGDROPOBJ) type ref to CL_DRAGDROPOBJECT
      value(ET_FLAVORS) type CNDD_FLAVORS .
  events DRAG
    exporting
      value(EV_ROW) type I
      value(EV_COLUMN) type LVC_FNAME
      value(ES_ROW_NO) type LVC_S_ROID
      value(ER_DRAGDROPOBJ) type ref to CL_DRAGDROPOBJECT .
  events DROP
    exporting
      value(EV_ROW) type I
      value(EV_COLUMN) type LVC_FNAME
      value(ES_ROW_NO) type LVC_S_ROID
      value(ER_DRAGDROPOBJ) type ref to CL_DRAGDROPOBJECT .
  events DROP_COMPLETE
    exporting
      value(EV_ROW) type I
      value(EV_COLUMN) type LVC_FNAME
      value(ES_ROW_NO) type LVC_S_ROID
      value(ER_DRAGDROPOBJ) type ref to CL_DRAGDROPOBJECT .
  events CONTEXT_MENU
    exporting
      value(ER_MENU) type ref to CL_CTMENU .
protected section.

  methods RAISE_DROP_GET_FLAVOR
    importing
      !IV_ROW type I
      !IV_COLUMN type LVC_FNAME
      !IS_ROW_NO type LVC_S_ROID
      !IR_DRAGDROPOBJ type ref to CL_DRAGDROPOBJECT
      !IT_FLAVORS type CNDD_FLAVORS optional .
  methods RAISE_DRAG
    importing
      !IV_ROW type I
      !IV_COLUMN type LVC_FNAME
      !IS_ROW_NO type LVC_S_ROID
      !IR_DRAGDROPOBJ type ref to CL_DRAGDROPOBJECT optional .
  methods RAISE_DROP
    importing
      !IV_ROW type I
      !IV_COLUMN type LVC_FNAME
      !IS_ROW_NO type LVC_S_ROID
      !IR_DRAGDROPOBJ type ref to CL_DRAGDROPOBJECT optional .
  methods RAISE_DROP_COMPLETE
    importing
      !IV_ROW type I
      !IV_COLUMN type LVC_FNAME
      !IS_ROW_NO type LVC_S_ROID
      !IR_DRAGDROPOBJ type ref to CL_DRAGDROPOBJECT optional .
  methods RAISE_FUNCTION_CHOSEN
    importing
      !IV_FUNCTION type UI_FUNC
      !IV_TAG type STRING optional .
  methods RAISE_AFTER_FUNCTION
    importing
      !IV_FUNCTION type UI_FUNC .
  methods RAISE_BEFORE_FUNCTION
    importing
      !IV_FUNCTION type UI_FUNC .
  methods RAISE_LINK_CLICK
    importing
      !IV_ROW type I
      !IV_COLUMN type LVC_FNAME .
  methods RAISE_DOUBLE_CLICK
    importing
      !IV_ROW type I
      !IV_COLUMN type LVC_FNAME .
  methods RAISE_F4
    importing
      !IV_FIELDNAME type LVC_FNAME
      !IV_FIELDVALUE type LVC_VALUE
      !IS_ROW_NO type LVC_S_ROID
      !IR_EVENT_DATA type ref to CL_ALV_EVENT_DATA
      !IT_BAD_CELLS type LVC_T_MODI optional
      !IF_DISPLAY type ABAP_BOOL optional .
  methods RAISE_DATA_CHANGED
    importing
      !IR_CHANGE_PROTOCOL type ref to CL_ALV_CHANGED_DATA_PROTOCOL optional
      !IF_ONF4 type ABAP_BOOL optional
      !IF_ONF4_BEFORE type ABAP_BOOL optional
      !IF_ONF4_AFTER type ABAP_BOOL optional
      !IV_FUNCTION type SY-UCOMM optional .
  methods RAISE_CONTEXT_MENU
    importing
      !IR_MENU type ref to CL_CTMENU .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_UITB_ALV_EVENTS IMPLEMENTATION.


  METHOD raise_after_function.
    RAISE EVENT after_function
      EXPORTING
        ev_function = iv_function.
  ENDMETHOD.


  METHOD raise_before_function.
    RAISE EVENT before_function
      EXPORTING
        ev_function = iv_function.
  ENDMETHOD.


  METHOD raise_context_menu.
    RAISE EVENT context_menu
      EXPORTING
        er_menu = ir_menu.
  ENDMETHOD.


  METHOD raise_data_changed.
    RAISE EVENT data_changed
      EXPORTING
        er_change_protocol = ir_change_protocol
        ef_onf4            = if_onf4
        ef_onf4_before     = if_onf4_before
        ef_onf4_after      = if_onf4_after
        ev_function        = iv_function.
  ENDMETHOD.


  METHOD raise_double_click.
    RAISE EVENT double_click
      EXPORTING
        ev_row    = iv_row
        ev_column = iv_column.
  ENDMETHOD.


  METHOD raise_drag.
    RAISE EVENT drag
      EXPORTING
        ev_row         = iv_row
        ev_column      = iv_column
        es_row_no      = is_row_no
        er_dragdropobj = ir_dragdropobj.
  ENDMETHOD.


  METHOD raise_drop.
    RAISE EVENT drop
      EXPORTING
        ev_row         = iv_row
        ev_column      = iv_column
        es_row_no      = is_row_no
        er_dragdropobj = ir_dragdropobj.
  ENDMETHOD.


  METHOD raise_drop_complete.
    RAISE EVENT drop_complete
      EXPORTING
        ev_row         = iv_row
        ev_column      = iv_column
        es_row_no      = is_row_no
        er_dragdropobj = ir_dragdropobj.
  ENDMETHOD.


  METHOD raise_drop_get_flavor.
    RAISE EVENT drop_get_flavor
      EXPORTING
        ev_row         = iv_row
        ev_column      = iv_column
        es_row_no      = is_row_no
        er_dragdropobj = ir_dragdropobj
        et_flavors     = it_flavors.
  ENDMETHOD.


  METHOD raise_f4.
    RAISE EVENT f4
      EXPORTING
        ev_fieldname  = iv_fieldname
        ev_fieldvalue = iv_fieldvalue
        es_row_no     = is_row_no
        er_event_data = ir_event_data
        et_bad_cells  = it_bad_cells
        ef_display    = if_display.
  ENDMETHOD.


  METHOD raise_function_chosen.
    RAISE EVENT function_chosen
      EXPORTING
        ev_function = iv_function
        ev_tag      = iv_tag.
  ENDMETHOD.


  METHOD raise_link_click.
    RAISE EVENT link_click
      EXPORTING
        ev_row    = iv_row
        ev_column = iv_column.
  ENDMETHOD.
ENDCLASS.
