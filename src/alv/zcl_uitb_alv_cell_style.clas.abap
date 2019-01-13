class ZCL_UITB_ALV_CELL_STYLE definition
  public
  final
  create public .

public section.

  methods SET_MAX_LENGTH
    importing
      !VALUE type INT4 .
  methods SET_STYLE
    importing
      !VALUE type LVC_STYLE .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_fieldname TYPE fieldname.
    DATA mv_style TYPE lvc_style.
    DATA mv_maxlen TYPE lvc_s_styl-maxlen.
ENDCLASS.



CLASS ZCL_UITB_ALV_CELL_STYLE IMPLEMENTATION.


  method SET_MAX_LENGTH.
    mv_style = value.
  endmethod.


  METHOD set_style.
    mv_style = value.
  ENDMETHOD.
ENDCLASS.
