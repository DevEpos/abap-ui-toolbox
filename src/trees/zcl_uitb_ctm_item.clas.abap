"! <p class="shorttext synchronized" lang="en">Item of a Node of a tree model</p>
CLASS zcl_uitb_ctm_item DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: ty_t_ctm_item TYPE STANDARD TABLE OF REF TO zcl_uitb_ctm_item WITH EMPTY KEY.

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        !ir_model           TYPE REF TO cl_column_tree_model
        !iv_node_key        TYPE tm_nodekey
        !iv_item_name       TYPE tv_itmname
        !is_item_properties TYPE treemcitem .
    "! <p class="shorttext synchronized" lang="en">Sets text property of item</p>
    METHODS set_text
      IMPORTING
        !value TYPE tm_itemtxt
      RAISING
        zcx_uitb_tree_error.
    "! <p class="shorttext synchronized" lang="en">Gets text property of item</p>
    METHODS get_text
      RETURNING
        VALUE(result) TYPE tm_itemtxt
      RAISING
        zcx_uitb_tree_error.
    "! <p class="shorttext synchronized" lang="en">Sets style property of item</p>
    METHODS set_style
      IMPORTING
        !value TYPE i
      RAISING
        zcx_uitb_tree_error.
    "! <p class="shorttext synchronized" lang="en">Gets style property of item</p>
    METHODS get_style
      RETURNING
        VALUE(result) TYPE i .
    "! <p class="shorttext synchronized" lang="en">Sets font property of item</p>
    METHODS set_font
      IMPORTING
        !value TYPE i
      RAISING
        zcx_uitb_tree_error.
    "! <p class="shorttext synchronized" lang="en">Gets font property of item</p>
    METHODS get_font
      RETURNING
        VALUE(result) TYPE i .
    "! <p class="shorttext synchronized" lang="en">Sets hidden property of item</p>
    METHODS set_hidden
      IMPORTING
        !value TYPE abap_bool DEFAULT abap_true
      RAISING
        zcx_uitb_tree_error.
    "! <p class="shorttext synchronized" lang="en">Gets hidden property of item</p>
    METHODS is_hidden
      RETURNING
        VALUE(result) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Sets chosen property of item</p>
    METHODS set_chosen
      IMPORTING
        !value TYPE abap_bool DEFAULT abap_true
      RAISING
        zcx_uitb_tree_error.
    "! <p class="shorttext synchronized" lang="en">Gets chosen property of item</p>
    METHODS is_chosen
      RETURNING
        VALUE(result) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Sets editable of item</p>
    METHODS set_editable
      IMPORTING
        !value TYPE abap_bool DEFAULT abap_true
      RAISING
        zcx_uitb_tree_error.
    "! <p class="shorttext synchronized" lang="en">Gets editable of item</p>
    METHODS is_editable
      RETURNING
        VALUE(result) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Sets disabled of item</p>
    METHODS set_disabled
      IMPORTING
        !value TYPE abap_bool DEFAULT abap_true
      RAISING
        zcx_uitb_tree_error.
    "! <p class="shorttext synchronized" lang="en">Gets disabled of item</p>
    METHODS is_disabled
      RETURNING
        VALUE(result) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Sets image of item</p>
    METHODS set_image
      IMPORTING
        !value TYPE tv_image
      RAISING
        zcx_uitb_tree_error.
    "! <p class="shorttext synchronized" lang="en">Gets image of item</p>
    METHODS get_image
      RETURNING
        VALUE(result) TYPE tv_image .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_node_key TYPE tm_nodekey .
    DATA mv_item_name TYPE tv_itmname .
    DATA mr_model TYPE REF TO cl_column_tree_model .
    DATA ms_item TYPE treemcitem .
ENDCLASS.



CLASS zcl_uitb_ctm_item IMPLEMENTATION.


  METHOD constructor.
    mr_model = ir_model.
    mv_node_key = iv_node_key.
    mv_item_name = iv_item_name.
    ms_item = is_item_properties.
  ENDMETHOD.


  METHOD get_font.
    result = ms_item-font.
  ENDMETHOD.


  METHOD get_image.
    result = ms_item-t_image.
  ENDMETHOD.


  METHOD get_style.
    result = ms_item-style.
  ENDMETHOD.


  METHOD get_text.
    mr_model->item_get_text(
      EXPORTING
        node_key       = mv_node_key
        item_name      = mv_item_name
      IMPORTING
        text           = result
      EXCEPTIONS
        node_not_found = 1
        item_not_found = 2
        OTHERS         = 3 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_uitb_tree_error.
    ENDIF.
  ENDMETHOD.


  METHOD is_chosen.
    result = ms_item-chosen.
  ENDMETHOD.


  METHOD is_disabled.
    result = ms_item-disabled.
  ENDMETHOD.


  METHOD is_editable.
    result = ms_item-editable.
  ENDMETHOD.


  METHOD is_hidden.
    result = ms_item-hidden.
  ENDMETHOD.


  METHOD set_chosen.

    mr_model->item_set_chosen(
      EXPORTING
        node_key             = mv_node_key
        item_name            = mv_item_name
        chosen               = value
      EXCEPTIONS
        node_not_found       = 1
        item_not_found       = 2
        chosen_not_supported = 3
        OTHERS               = 4 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_uitb_tree_error.
    ELSE.
      ms_item-chosen = value.
    ENDIF.

  ENDMETHOD.


  METHOD set_disabled.

    mr_model->item_set_disabled(
      EXPORTING
        node_key          = mv_node_key
        item_name         = mv_item_name
        disabled          = value
      EXCEPTIONS
        node_not_found    = 1
        item_not_found    = 2
        no_item_selection = 3
        OTHERS            = 4 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_uitb_tree_error.
    ELSE.
      ms_item-disabled = value.
    ENDIF.

  ENDMETHOD.


  METHOD set_editable.

    mr_model->item_set_editable(
      EXPORTING
        node_key               = mv_node_key
        item_name              = mv_item_name
        editable               = value
      EXCEPTIONS
        node_not_found         = 1
        item_not_found         = 2
        editable_not_supported = 3
        no_item_selection      = 4
        OTHERS                 = 5 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_uitb_tree_error.
    ELSE.
      ms_item-editable = value.
    ENDIF.

  ENDMETHOD.


  METHOD set_font.

    mr_model->item_set_font(
      EXPORTING
        node_key       = mv_node_key
        item_name      = mv_item_name
        font           = value
      EXCEPTIONS
        node_not_found = 1
        item_not_found = 2
        OTHERS         = 3 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_uitb_tree_error.
    ELSE.
      ms_item-font = value.
    ENDIF.

  ENDMETHOD.


  METHOD set_hidden.

    mr_model->item_set_hidden(
      EXPORTING
        node_key       = mv_node_key
        item_name      = mv_item_name
        hidden         = value
      EXCEPTIONS
        node_not_found = 1
        item_not_found = 2
        OTHERS         = 3 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_uitb_tree_error.
    ELSE.
      ms_item-hidden = value.
    ENDIF.

  ENDMETHOD.


  METHOD set_image.

    mr_model->item_set_image(
      EXPORTING
        node_key       = mv_node_key
        item_name      = mv_item_name
        image          = value
      EXCEPTIONS
        node_not_found = 1
        item_not_found = 2
        OTHERS         = 3 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_uitb_tree_error.
    ELSE.
      ms_item-t_image = value.
    ENDIF.

  ENDMETHOD.


  METHOD set_style.

    mr_model->item_set_style(
      EXPORTING
        node_key       = mv_node_key
        item_name      = mv_item_name
        style          = value
      EXCEPTIONS
        node_not_found = 1
        item_not_found = 2
        OTHERS         = 3 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_uitb_tree_error.
    ELSE.
      ms_item-style = value.
    ENDIF.

  ENDMETHOD.


  METHOD set_text.
    mr_model->item_set_text(
      EXPORTING
        node_key       = mv_node_key
        item_name      = mv_item_name
        text           = value
      EXCEPTIONS
        node_not_found = 1
        item_not_found = 2
        OTHERS         = 3 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_uitb_tree_error.
    ELSE.
      ms_item-text = value.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
