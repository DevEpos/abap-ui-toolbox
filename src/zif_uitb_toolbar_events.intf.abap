INTERFACE ZIF_UITB_toolbar_events
  PUBLIC .
  EVENTS function_selected
    EXPORTING
      VALUE(ev_fcode) TYPE ui_func .

  EVENTS dropdown_clicked
    EXPORTING
      VALUE(ev_fcode) TYPE ui_func.
ENDINTERFACE.
