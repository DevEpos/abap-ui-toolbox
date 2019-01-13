CLASS ZCL_UITB_alv_func_settings DEFINITION
  PUBLIC
  INHERITING FROM ZCL_UITB_alv_metadata
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        ir_controller TYPE REF TO ZIF_UITB_alv_metadata_ctrller.
    METHODS get_dropdowns
      RETURNING
        VALUE(result) TYPE REF TO ZCL_UITB_alv_dropdowns.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mr_dropdowns TYPE REF TO ZCL_UITB_alv_dropdowns.
ENDCLASS.



CLASS ZCL_UITB_alv_func_settings IMPLEMENTATION.

  METHOD constructor.
    super->constructor(
      ir_controller = ir_controller
      iv_name       = 'FUCTIONAL_SETTINGS'
    ).
  ENDMETHOD.

  METHOD get_dropdowns.
    IF mr_dropdowns IS INITIAL.
      mr_dropdowns = NEW #( ir_controller = mr_controller ).
    ENDIF.

    result = mr_dropdowns.
  ENDMETHOD.

ENDCLASS.
