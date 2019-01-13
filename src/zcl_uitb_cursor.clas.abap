class ZCL_UITB_CURSOR definition
  public
  final
  create private .

public section.

  class-methods GET_CURSOR
    importing
      !IF_RESET type BOOLEAN default ABAP_TRUE
    returning
      value(RR_CURSOR) type ref to ZCL_UITB_CURSOR .
  methods GET_FIELD
    returning
      value(RV_FIELD) type DYNFNAM .
  methods GET_VALUE
    returning
      value(RV_VALUE) type DYNFNAM .
  methods GET_LENGTH
    returning
      value(RV_LENGTH) type I .
  methods GET_OFFSET
    returning
      value(RV_OFFSET) type I .
  methods GET_LINE
    returning
      value(RV_LINE) type I .
  methods GET_AREA
    returning
      value(RV_AREA) type DYNFNAM .
  methods POSITIONED_IN_AREA
    importing
      !IV_AREA type DYNFNAM
    returning
      value(RF_IN_AREA) type BOOLEAN .
  methods SET_LINE
    importing
      !IV_LINE type I .
  methods SET_FIELD
    importing
      !IV_FIELD type DYNFNAM .
  class-methods REFRESH_CURSOR .
  class-methods SET_CURSOR_FOR_OBJECT
    importing
      !IR_CURSOR type ref to ZCL_UITB_CURSOR .
  class-methods SET_CURSOR
    importing
      !IV_FIELD type DYNFNAM
      !IV_LINE type I optional .
  methods SET_IS_UPDATED
    importing
      !VALUE type ABAP_BOOL default ABAP_TRUE .
  methods IS_UPDATED
    returning
      value(RESULT) type ABAP_BOOL .
  methods REQUEST_UPDATE .
  methods REFRESH .
  methods IS_UPDATE_REQUESTED
    returning
      value(RESULT) type ABAP_BOOL .
  PROTECTED SECTION.
private section.

  data MV_FIELD type DYNFNAM .
  data MV_VALUE type DYNFNAM .
  data MV_LENGTH type I .
  data MV_OFFSET type I .
  data MV_LINE type I .
  data MV_AREA type DYNFNAM .
  class-data SR_CURSOR type ref to ZCL_UITB_CURSOR .
  data MF_UPDATED type ABAP_BOOL .
  data MF_UPDATE_REQUESTED type ABAP_BOOL .

  methods CONSTRUCTOR
    importing
      !IV_FIELD type DYNFNAM
      !IV_VALUE type DYNFNAM optional
      !IV_LENGTH type I optional
      !IV_OFFSET type I optional
      !IV_LINE type I optional
      !IV_AREA type DYNFNAM optional .
ENDCLASS.



CLASS ZCL_UITB_CURSOR IMPLEMENTATION.


  METHOD constructor.
    mv_field  = iv_field.
    mv_value  = iv_value.
    mv_length = iv_length.
    mv_offset = iv_offset.
    mv_line   = iv_line.
    mv_area   = iv_area.
  ENDMETHOD.


  METHOD get_area.
    rv_area = mv_area.
  ENDMETHOD.


  METHOD get_cursor.
    IF if_reset = abap_false AND sr_cursor IS BOUND.
      rr_cursor = sr_cursor.
    ELSE.
      DATA: lv_field  TYPE dynfnam,
            lv_value  TYPE dynfnam,
            lv_length TYPE i,
            lv_offset TYPE i,
            lv_line   TYPE i,
            lv_area   TYPE dynfnam.

      GET CURSOR FIELD  lv_field
                 VALUE  lv_value
                 LENGTH lv_length
                 OFFSET lv_offset
                 LINE   lv_line
                 AREA   lv_area.

      sr_cursor = NEW zcl_uitb_cursor(
          iv_field  = lv_field
          iv_value  = lv_value
          iv_length = lv_length
          iv_offset = lv_offset
          iv_line   = lv_line
          iv_area   = lv_area
      ).
      rr_cursor = sr_cursor.
    ENDIF.
  ENDMETHOD.


  METHOD get_field.
    rv_field = mv_field.
  ENDMETHOD.


  METHOD get_length.
    rv_length = mv_length.
  ENDMETHOD.


  METHOD get_line.
    rv_line = mv_line.
  ENDMETHOD.


  METHOD get_offset.
    rv_offset = mv_offset.
  ENDMETHOD.


  METHOD get_value.
    rv_value = mv_value.
  ENDMETHOD.


  METHOD is_updated.
    result = mf_updated.
  ENDMETHOD.


  method IS_UPDATE_REQUESTED.
    result = mf_update_requested.
  endmethod.


  METHOD positioned_in_area.
    rf_in_area = xsdbool( mv_area = iv_area ).
  ENDMETHOD.


  METHOD refresh.
    set_cursor_for_object( me ).
  ENDMETHOD.


  METHOD refresh_cursor.
    set_cursor_for_object( ir_cursor = get_cursor( if_reset = abap_false ) ).
  ENDMETHOD.


  METHOD request_update.
    mf_update_requested = abap_true.
  ENDMETHOD.


  METHOD set_cursor.
    IF iv_line <> 0.
      SET CURSOR FIELD iv_field LINE iv_line.
    ELSE.
      SET CURSOR FIELD iv_field.
    ENDIF.
  ENDMETHOD.


  METHOD set_cursor_for_object.
    DATA(lv_field) = ir_cursor->get_field( ).
    DATA(lv_line) = ir_cursor->get_line( ).

    IF lv_line <> 0.
      SET CURSOR FIELD lv_field LINE lv_line.
    ELSE.
      SET CURSOR FIELD lv_field.
    ENDIF.

    IF ir_cursor->is_update_requested( ).
      clear ir_cursor->mf_update_requested.
    ENDIF.
  ENDMETHOD.


  METHOD set_field.
    mv_field = iv_field.
  ENDMETHOD.


  METHOD set_is_updated.
    mf_updated = value.
  ENDMETHOD.


  METHOD set_line.
    mv_line = iv_line.
  ENDMETHOD.
ENDCLASS.
