CLASS ZCL_UITB_ctm_columns DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        ir_model TYPE REF TO cl_column_tree_model.
    METHODS add_hierarchy_column
      IMPORTING
        !iv_colname TYPE tv_itmname
      RAISING
        ZCX_UITB_tree_error .
    METHODS add_column
      IMPORTING
        !iv_colname        TYPE tv_itmname
        !iv_width          TYPE i OPTIONAL
        !iv_header_image   TYPE tv_image OPTIONAL
        !iv_header_text    TYPE tv_heading OPTIONAL
        !iv_header_tooltip TYPE tv_heading OPTIONAL
        iv_alignment type i optional
      RAISING
        ZCX_UITB_tree_error .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mr_model TYPE REF TO cl_column_tree_model.
ENDCLASS.



CLASS ZCL_UITB_ctm_columns IMPLEMENTATION.
  METHOD constructor.
    mr_model = ir_model.
  ENDMETHOD.

  METHOD add_hierarchy_column.
    mr_model->add_hierarchy_column(
     EXPORTING  name                = iv_colname
     EXCEPTIONS column_exists       = 1
                illegal_column_name = 2
                too_many_columns    = 3
                OTHERS              = 4
    ).

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ZCX_UITB_tree_error.
    ENDIF.
  ENDMETHOD.

  METHOD add_column.

    mr_model->add_column(
      EXPORTING
        name                = iv_colname
*        hidden              =     " 'X': unsichtbar
*        disabled            =     " 'X': not selectable
        alignment           = iv_alignment
        width               = iv_width
        header_image        = iv_header_image
        header_text         = iv_header_text
        header_tooltip      = iv_header_tooltip
      EXCEPTIONS
        column_exists       = 1
        illegal_column_name = 2
        too_many_columns    = 3
        illegal_alignment   = 4
        OTHERS              = 5
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ZCX_UITB_tree_error.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
