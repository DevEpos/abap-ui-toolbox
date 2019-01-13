interface ZIF_UITB_EXIT_CALLBACK
  public .


  methods CANCEL_EXIT .
  methods EXIT_CANCELLED
    returning
      value(RF_EXIT_CANCELLED) type SAP_BOOL .
endinterface.
