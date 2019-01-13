interface ZIF_UITB_ALV_FUNCTION_HOLDER
  public .


  events FUNCTION_REGISTERED
    exporting
      value(ES_FUNCTION) type ZIF_UITB_ALV_TYPES=>TY_ALV_FUNCTION .
  events UNREGISTER_FUNCTIONS
    exporting
      value(EF_TOOLBAR) type ABAP_BOOL optional
      value(EF_CONTEXT_MENU) type ABAP_BOOL optional .
  events UNREGISTER_FUNCTION
    exporting
      value(ES_FUNCTION) type ZIF_UITB_ALV_TYPES=>TY_ALV_FUNCTION .
endinterface.
