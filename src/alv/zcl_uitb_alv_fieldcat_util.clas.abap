class ZCL_UITB_ALV_FIELDCAT_UTIL definition
  public
  final
  create public .

public section.

  class-methods CREATE_FIELD_CATALOG
    importing
      !IR_TABLE type ref to DATA
      !IV_FIELD_LANGUAGE type LANGU default SY-LANGU
      !IF_EDITABLE type ABAP_BOOL optional
    returning
      value(RESULT) type LVC_T_FCAT .
protected section.
private section.
ENDCLASS.



CLASS ZCL_UITB_ALV_FIELDCAT_UTIL IMPLEMENTATION.


  METHOD create_field_catalog.
*... create alv object to get all information about the fields in the table
    DATA(lr_alv) = zcl_uitb_alv=>create_alv(
        ir_data                 = ir_table
        iv_description_language = iv_field_language
        ir_container            = cl_gui_container=>screen0
        if_editable             = if_editable
    ).

*... retrieve field catalog information
    result = zcl_uitb_alv_metadata_util=>set_fieldcatalog(
        ir_columns          = lr_alv->get_columns( )
        ir_display_settings = lr_alv->get_display_settings( )
    ).
  ENDMETHOD.
ENDCLASS.
