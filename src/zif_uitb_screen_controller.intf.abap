interface ZIF_UITB_SCREEN_CONTROLLER
  public .


  data MF_NEW_SEQUENCE type ABAP_BOOL .
  data MF_FIRST_CALL type ABAP_BOOL .

  events LEAVE .

  methods HANDLE_USER_COMMAND
    changing
      !CV_FUNCTION_CODE type SY-UCOMM .
  methods LOAD_CONTEXT_MENU default ignore
    importing
      !IR_MENU type ref to CL_CTMENU
      !IV_SCREEN_PARAMETER type STRING optional .
  methods DETERMINE_CURSOR default ignore .
  methods PBO .
  methods SET_STATUS .
  methods CALL_SCREEN
    importing
      !IF_AS_DIALOG type ABAP_BOOL optional .
  methods FREE_SCREEN_RESOURCES default ignore .
  methods CANCEL default ignore
    importing
      value(IV_FUNCTION_CODE) type SY-UCOMM optional .
  methods WAS_NOT_CANCELLED default ignore
    returning
      value(RF_NOT_CANCELLED) type BOOLEAN .
  methods GET_REPORT_ID default ignore
    returning
      value(RESULT) type REPID .
  methods GET_SCREEN_ID default ignore
    returning
      value(RESULT) type DYNNR .
endinterface.
