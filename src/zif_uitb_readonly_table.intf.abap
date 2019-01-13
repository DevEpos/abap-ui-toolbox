interface ZIF_UITB_READONLY_TABLE
  public .


  methods UPDATE_FIELDS
    importing
      !IV_FUNCTION type SY-UCOMM optional .
  methods DETERMINE_LINE_COUNT .
  methods DETERMINE_CURRENT_LINE .
endinterface.
