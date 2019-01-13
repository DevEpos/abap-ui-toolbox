interface ZIF_UITB_VIEW
  public .


  data MF_VISIBLE type ABAP_BOOL .

  methods SHOW
    importing
      !IV_START_LINE type I optional
      !IV_START_COLUMN type I optional
      !IV_END_LINE type I optional
      !IV_END_COLUMN type I optional .
  methods IS_VISIBLE default ignore
    returning
      value(RESULT) type ABAP_BOOL .
  methods HIDE default ignore .
  methods FREE default ignore .
endinterface.
