interface ZIF_UITB_TABLE
  public .


  methods ADD_LINE
    importing
      !IF_INSERT type BOOLEAN optional
    returning
      value(RV_NEW_INDEX) type SY-TABIX .
  methods UPDATE_FIELDS
    importing
      !IV_FUNCTION_CODE type SY-UCOMM optional .
  methods UPDATE_SCREEN_ATTRIBUTES .
  methods DETERMINE_LINE_COUNT .
  methods DETERMINE_CURRENT_LINE .
  methods GET_CURRENT_LINE_INDEX
    returning
      value(RV_INDEX) type SY-TABIX .
  methods DELETE_CURRENT_LINE .
  methods GET_CURRENT_LINE_VALUE default ignore
    exporting
      !ES_LINE type ANY .
  methods GET_CURRENT_LINE_REF default ignore
    returning
      value(RR_LINE) type ref to DATA .
  methods DELETE_ALL .
  methods GET_CURRENT_LOOP_LINE default ignore
    returning
      value(RV_CURRENT_LOOP_LINE) type SY-TABIX .
  methods GET_TABLE_DATA default ignore
    returning
      value(RR_TABLE_DATA_ITAB) type ref to DATA .
  methods PBO default ignore .
endinterface.
