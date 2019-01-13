class ZCL_UITB_ALV_COLOR_MAPPER definition
  public
  final
  create public .

public section.

  class-methods STRING_TO_STRUC
    importing
      !IV_COLOR type CHAR4
    returning
      value(RESULT) type LVC_S_COLO .
protected section.
private section.
ENDCLASS.



CLASS ZCL_UITB_ALV_COLOR_MAPPER IMPLEMENTATION.


  METHOD string_to_struc.
    result-col = iv_color+1(1).
    result-int = iv_color+2(1).
    result-inv = iv_color+3(1).
  ENDMETHOD.
ENDCLASS.
