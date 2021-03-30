"! <p class="shorttext synchronized" lang="en">Node of a column tree model</p>
CLASS zcl_uitb_ctm_node DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: ty_t_ctm_node TYPE STANDARD TABLE OF REF TO zcl_uitb_ctm_node WITH EMPTY KEY.
    "! <p class="shorttext synchronized" lang="en">Tree Control: Node Key</p>
    DATA mv_node_key TYPE tm_nodekey READ-ONLY .

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        !iv_node_key   TYPE tm_nodekey
        !is_properties TYPE treemsnod OPTIONAL
        ir_user_data   TYPE REF TO data OPTIONAL
        !ir_model      TYPE REF TO cl_column_tree_model
        !ir_nodes      TYPE REF TO zcl_uitb_ctm_nodes .
    "! <p class="shorttext synchronized" lang="en">Deletes all child nodes</p>
    METHODS delete_children
      RAISING
        zcx_uitb_tree_error .
    "! <p class="shorttext synchronized" lang="en">Get all child nodes</p>
    METHODS get_children
      RETURNING
        VALUE(rt_children) TYPE ty_t_ctm_node
      RAISING
        zcx_uitb_tree_error .
    "! <p class="shorttext synchronized" lang="en">Gets expanded image property of node</p>
    METHODS get_expanded_image
      RETURNING
        VALUE(result) TYPE tv_image .
    "! <p class="shorttext synchronized" lang="en">Gets the first child of a node</p>
    METHODS get_first_child
      RETURNING
        VALUE(result) TYPE REF TO zcl_uitb_ctm_node
      RAISING
        zcx_uitb_tree_error .
    "! <p class="shorttext synchronized" lang="en">Gets image property of node</p>
    METHODS get_image
      RETURNING
        VALUE(result) TYPE tv_image .
    "! <p class="shorttext synchronized" lang="en">Get a specific item of this node</p>
    METHODS get_item
      IMPORTING
        !iv_item_name  TYPE tv_itmname
      RETURNING
        VALUE(rr_item) TYPE REF TO zcl_uitb_ctm_item
      RAISING
        zcx_uitb_tree_error .
    "! <p class="shorttext synchronized" lang="en">Get all items of this node</p>
    METHODS get_items
      RETURNING
        VALUE(result) TYPE zcl_uitb_ctm_item=>ty_t_ctm_item
      RAISING
        zcx_uitb_tree_error .
    "! <p class="shorttext synchronized" lang="en">gets the last child of a node</p>
    METHODS get_last_child
      RETURNING
        VALUE(result) TYPE REF TO zcl_uitb_ctm_node
      RAISING
        zcx_uitb_tree_error .
    "! <p class="shorttext synchronized" lang="en">Gets the next sibling of the node</p>
    METHODS get_next_sibling
      RETURNING
        VALUE(result) TYPE REF TO zcl_uitb_ctm_node
      RAISING
        zcx_uitb_tree_error .
    "! <p class="shorttext synchronized" lang="en">Get number of children property</p>
    METHODS get_nr_of_children
      RETURNING
        VALUE(result) TYPE i
      RAISING
        zcx_uitb_tree_error .
    "! <p class="shorttext synchronized" lang="en">Gets parent node</p>
    METHODS get_parent
      RETURNING
        VALUE(result) TYPE REF TO zcl_uitb_ctm_node
      RAISING
        zcx_uitb_tree_error .
    "! <p class="shorttext synchronized" lang="en">Retrieves root key</p>
    METHODS get_root_key
      RETURNING
        VALUE(rv_root_key) TYPE tm_nodekey.
    "! <p class="shorttext synchronized" lang="en">Gets the previous sibling of a node</p>
    METHODS get_previous_sibling
      RETURNING
        VALUE(result) TYPE REF TO zcl_uitb_ctm_node
      RAISING
        zcx_uitb_tree_error .
    "! <p class="shorttext synchronized" lang="en">Gets properties of node</p>
    METHODS get_properties
      RETURNING
        VALUE(result) TYPE treemsnod
      RAISING
        zcx_uitb_tree_error .
    "! <p class="shorttext synchronized" lang="en">Gets user object property of node</p>
    METHODS get_user_object
      RETURNING
        VALUE(result) TYPE REF TO object
      RAISING
        zcx_uitb_tree_error .
    "! <p class="shorttext synchronized" lang="en">Checks if this node has child nodes</p>
    METHODS has_children
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        zcx_uitb_tree_error .
    "! <p class="shorttext synchronized" lang="en">Gets expander property of node</p>
    METHODS has_expander
      RETURNING
        VALUE(result) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Is this node expanded?</p>
    METHODS is_expanded
      RETURNING
        VALUE(result) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Gets disabled property of node</p>
    METHODS is_disabled
      RETURNING
        VALUE(result) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Checks if this node is its parent's first child</p>
    METHODS is_first_child
      RETURNING
        VALUE(rf_is_first_child) TYPE abap_bool
      RAISING
        zcx_uitb_tree_error .
    "! <p class="shorttext synchronized" lang="en">Gets "is folder" property of node</p>
    METHODS is_folder
      RETURNING
        VALUE(result) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Checks if this node is its parent's last child</p>
    METHODS is_last_child
      RETURNING
        VALUE(rf_is_last_child) TYPE abap_bool
      RAISING
        zcx_uitb_tree_error .
    "! <p class="shorttext synchronized" lang="en">Moves node to another node</p>
    METHODS move_node_to
      IMPORTING
        !iv_relative_node TYPE tm_nodekey
        !iv_relationship  TYPE i
      RAISING
        zcx_uitb_tree_error .
    "! <p class="shorttext synchronized" lang="en">Sets disabled property of node</p>
    METHODS set_disabled
      IMPORTING
        !value TYPE abap_bool DEFAULT abap_true
      RAISING
        zcx_uitb_tree_error .
    "! <p class="shorttext synchronized" lang="en">Sets expanded image property of node</p>
    METHODS set_expanded_image
      IMPORTING
        !value TYPE tv_image
      RAISING
        zcx_uitb_tree_error .
    "! <p class="shorttext synchronized" lang="en">Sets expander property of node</p>
    METHODS set_expander
      IMPORTING
        !value TYPE abap_bool DEFAULT abap_true
      RAISING
        zcx_uitb_tree_error .
    "! <p class="shorttext synchronized" lang="en">Sets folder property of node</p>
    METHODS set_folder
      IMPORTING
        !value TYPE abap_bool DEFAULT abap_true
      RAISING
        zcx_uitb_tree_error .
    "! <p class="shorttext synchronized" lang="en">Sets image property of node</p>
    METHODS set_image
      IMPORTING
        !value TYPE tv_image
      RAISING
        zcx_uitb_tree_error .
    "! <p class="shorttext synchronized" lang="en">Sets style property of node</p>
    METHODS set_style
      IMPORTING
        !iv_style TYPE i
      RAISING
        zcx_uitb_tree_error .
    "! <p class="shorttext synchronized" lang="en">Sets user object property of node</p>
    METHODS set_user_object
      IMPORTING
        !value TYPE REF TO object
      RAISING
        zcx_uitb_tree_error .
    "! <p class="shorttext synchronized" lang="en">Either expands or colllapse node if it is a folder</p>
    METHODS toggle .
    "! <p class="shorttext synchronized" lang="en">Gets next node (siblings first then parent)</p>
    METHODS get_next_relat_node
      EXPORTING
        !ef_is_sibling TYPE abap_bool
      RETURNING
        VALUE(rr_node) TYPE REF TO zcl_uitb_ctm_node .
    "! <p class="shorttext synchronized" lang="en">Retrieve user data of node</p>
    "!
    METHODS get_user_data
      RETURNING
        VALUE(result) TYPE REF TO data.
    "! <p class="shorttext synchronized" lang="en">Set user data of node</p>
    "!
    METHODS set_user_data
      IMPORTING
        value TYPE REF TO data.
  PROTECTED SECTION.
  PRIVATE SECTION.
    "! <p class="shorttext synchronized" lang="en">User data</p>
    DATA mr_user_data TYPE REF TO data.
    "! <p class="shorttext synchronized" lang="en">Column Tree Control with Data Management at Backend</p>
    DATA mr_model TYPE REF TO cl_column_tree_model .
    "! <p class="shorttext synchronized" lang="en">Nodes of Column Tree Model</p>
    DATA mr_nodes TYPE REF TO zcl_uitb_ctm_nodes .
    "! <p class="shorttext synchronized" lang="en">Tree Model: Attributes of a Node</p>
    DATA ms_properties TYPE treemsnod .
ENDCLASS.



CLASS zcl_uitb_ctm_node IMPLEMENTATION.


  METHOD constructor.
    mv_node_key = iv_node_key.
    mr_model = ir_model.
    mr_nodes = ir_nodes.
    ms_properties = is_properties.

    IF ms_properties IS INITIAL.
      TRY.
          get_properties( ).
        CATCH zcx_uitb_tree_error ##needed.
*....... should not happen as the node object will not be created if there
*....... is no node for this key
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD delete_children.
    mr_model->node_get_children(
      EXPORTING
        node_key       = mv_node_key
      IMPORTING
        node_key_table = DATA(lt_children)
      EXCEPTIONS
        node_not_found = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_uitb_tree_error.
    ELSE.
      CHECK lt_children IS NOT INITIAL.
      mr_model->delete_nodes( lt_children ).
    ENDIF.
  ENDMETHOD.


  METHOD get_children.
    mr_model->node_get_children(
      EXPORTING
        node_key       = mv_node_key
      IMPORTING
        node_key_table = DATA(lt_children)
      EXCEPTIONS
        node_not_found = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_uitb_tree_error.
    ELSE.
      CHECK lt_children IS NOT INITIAL.

      LOOP AT lt_children ASSIGNING FIELD-SYMBOL(<lv_node>).
        rt_children = VALUE #( BASE rt_children
          ( NEW zcl_uitb_ctm_node(
              iv_node_key   = <lv_node>
              ir_model      = mr_model
              ir_nodes      = mr_nodes ) ) ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD get_expanded_image.
    result = ms_properties-exp_image.
  ENDMETHOD.


  METHOD get_first_child.
    mr_model->node_get_first_child(
      EXPORTING
        node_key        = mv_node_key
      IMPORTING
        child_node_key  = DATA(lv_child)
      EXCEPTIONS
        node_not_found  = 1
        OTHERS          = 2 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_uitb_tree_error.
    ELSE.
      CHECK lv_child IS NOT INITIAL.
      result = NEW #(
        iv_node_key = lv_child
        ir_model    = mr_model
        ir_nodes    = mr_nodes ).
    ENDIF.
  ENDMETHOD.


  METHOD get_image.
    result = ms_properties-n_image.
  ENDMETHOD.


  METHOD get_item.
    mr_model->node_get_item(
      EXPORTING
        node_key       = mv_node_key
        item_name      = iv_item_name
      IMPORTING
        item           = DATA(ls_item)
      EXCEPTIONS
        node_not_found = 1
        item_not_found = 2
        OTHERS         = 3 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_uitb_tree_error.
    ELSE.
      rr_item = NEW zcl_uitb_ctm_item(
        ir_model           = mr_model
        iv_node_key        = mv_node_key
        iv_item_name       = iv_item_name
        is_item_properties = ls_item ).
    ENDIF.
  ENDMETHOD.


  METHOD get_items.
    mr_model->node_get_items(
      EXPORTING
        node_key       = mv_node_key
      IMPORTING
        item_table     = DATA(lt_items)
      EXCEPTIONS
        node_not_found = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_uitb_tree_error.
    ELSE.
      LOOP AT lt_items ASSIGNING FIELD-SYMBOL(<ls_item>).
        result = VALUE #(
          BASE result
          ( NEW zcl_uitb_ctm_item(
              ir_model           = mr_model
              iv_node_key        = mv_node_key
              iv_item_name       = <ls_item>-item_name
              is_item_properties = <ls_item> ) ) ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD get_last_child.
    mr_model->node_get_last_child(
      EXPORTING
        node_key        = mv_node_key
      IMPORTING
        child_node_key  = DATA(lv_child)
      EXCEPTIONS
        node_not_found  = 1
        OTHERS          = 2 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_uitb_tree_error.
    ELSE.
      CHECK lv_child IS NOT INITIAL.
      result = NEW #(
        iv_node_key = lv_child
        ir_model    = mr_model
        ir_nodes    = mr_nodes ).
    ENDIF.
  ENDMETHOD.


  METHOD get_next_relat_node.
    ef_is_sibling = abap_true.

    DATA(lr_next_node) = get_previous_sibling( ).
    IF lr_next_node IS INITIAL.
      lr_next_node = get_next_sibling( ).
      IF lr_next_node IS INITIAL.
        lr_next_node = get_parent( ).
        CLEAR ef_is_sibling.
      ENDIF.
    ENDIF.

    rr_node = lr_next_node.
  ENDMETHOD.


  METHOD get_next_sibling.
    mr_model->node_get_next_sibling(
      EXPORTING
        node_key        = mv_node_key
      IMPORTING
        sibling_node_key = DATA(lv_sibling)
      EXCEPTIONS
        node_not_found  = 1
        OTHERS          = 2 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_uitb_tree_error.
    ELSE.
      CHECK lv_sibling IS NOT INITIAL.

      result = NEW #(
        iv_node_key = lv_sibling
        ir_model    = mr_model
        ir_nodes    = mr_nodes ).
    ENDIF.
  ENDMETHOD.


  METHOD get_nr_of_children.
    mr_model->node_get_nr_of_children(
      EXPORTING
        node_key       = mv_node_key
      IMPORTING
        nr_of_children = result
      EXCEPTIONS
        node_not_found = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_uitb_tree_error.
    ENDIF.
  ENDMETHOD.


  METHOD get_parent.
    mr_model->node_get_parent(
      EXPORTING
        node_key        = mv_node_key
      IMPORTING
        parent_node_key = DATA(lv_parent)
      EXCEPTIONS
        node_not_found  = 1
        OTHERS          = 2 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_uitb_tree_error.
    ELSE.
      CHECK lv_parent IS NOT INITIAL.
      result = NEW #(
        iv_node_key = lv_parent
        ir_model    = mr_model
        ir_nodes    = mr_nodes ).
    ENDIF.
  ENDMETHOD.

  METHOD get_root_key.
    CLEAR rv_root_key.
    DATA(lv_node_key) = mv_node_key.

    WHILE rv_root_key IS INITIAL.
      mr_model->node_get_parent(
        EXPORTING  node_key        = lv_node_key
        IMPORTING  parent_node_key = DATA(lv_parent)
        EXCEPTIONS node_not_found  = 1
                   OTHERS          = 2 ).
      IF sy-subrc = 0.
        IF lv_parent IS INITIAL.
          rv_root_key = lv_node_key.
        ELSE.
          lv_node_key = lv_parent.
        ENDIF.
      ELSE.
        RETURN.
      ENDIF.
    ENDWHILE.
  ENDMETHOD.


  METHOD get_previous_sibling.
    mr_model->node_get_prev_sibling(
      EXPORTING
        node_key        = mv_node_key
      IMPORTING
        sibling_node_key = DATA(lv_sibling)
      EXCEPTIONS
        node_not_found  = 1
        OTHERS          = 2 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_uitb_tree_error.
    ELSE.
      CHECK lv_sibling IS NOT INITIAL.
      result = NEW #(
        iv_node_key = lv_sibling
        ir_model    = mr_model
        ir_nodes    = mr_nodes ).
    ENDIF.
  ENDMETHOD.


  METHOD get_properties.
    mr_model->node_get_properties(
      EXPORTING
        node_key       = mv_node_key
      IMPORTING
        properties     = result
      EXCEPTIONS
        node_not_found = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_uitb_tree_error.
    ELSE.
      ms_properties = result.
    ENDIF.
  ENDMETHOD.


  METHOD get_user_object.
    mr_model->node_get_user_object(
      EXPORTING
        node_key       = mv_node_key
      IMPORTING
        user_object    = result
      EXCEPTIONS
        node_not_found = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_uitb_tree_error.
    ENDIF.
  ENDMETHOD.


  METHOD has_children.
    result = xsdbool( get_nr_of_children( ) > 0 ).
  ENDMETHOD.


  METHOD has_expander.
    result = ms_properties-expander.
  ENDMETHOD.


  METHOD is_disabled.
    result = ms_properties-disabled.
  ENDMETHOD.


  METHOD is_expanded.
    CHECK is_folder( ).

    DATA(lt_expanded_nodes) = mr_nodes->get_expanded_nodes( ).
    IF line_exists( lt_expanded_nodes[ table_line = mv_node_key ] ).
      result = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD is_first_child.
    DATA(lr_parent) = get_parent( ).
*.. Does it have a parent at all?
    CHECK lr_parent IS BOUND.

*.. Retrieve the first child and check it check it against self
    DATA(lr_first_child) = lr_parent->get_first_child( ).

    rf_is_first_child = xsdbool( lr_first_child->mv_node_key = mv_node_key ).

  ENDMETHOD.


  METHOD is_folder.
    result = ms_properties-isfolder.
  ENDMETHOD.


  METHOD is_last_child.
    DATA(lr_parent) = get_parent( ).
*.. Does it have a parent at all?
    CHECK lr_parent IS BOUND.

*.. Retrieve the last child and check it check it against self
    DATA(lr_last_child) = lr_parent->get_last_child( ).

    rf_is_last_child = xsdbool( lr_last_child->mv_node_key = mv_node_key ).
  ENDMETHOD.


  METHOD move_node_to.
    mr_model->move_node(
      EXPORTING
        node_key                = mv_node_key
        relative_node_key       = iv_relative_node
        relationship            = iv_relationship
      EXCEPTIONS
        control_not_existing    = 1
        control_dead            = 2
        failed                  = 3
        cntl_system_error       = 4
        node_not_found          = 5
        move_error              = 6
        relative_node_not_found = 7
        illegal_relationship    = 8
        OTHERS                  = 9 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_uitb_tree_error.
    ENDIF.
  ENDMETHOD.


  METHOD set_disabled.
    mr_model->node_set_disabled(
      EXPORTING
        node_key       = mv_node_key
        disabled       = value
      EXCEPTIONS
        node_not_found = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_uitb_tree_error.
    ENDIF.

  ENDMETHOD.


  METHOD set_expanded_image.
    mr_model->node_set_expanded_image(
      EXPORTING
        node_key       = mv_node_key
        exp_image      = value
      EXCEPTIONS
        node_not_found = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_uitb_tree_error.
    ENDIF.

  ENDMETHOD.


  METHOD set_expander.
    mr_model->node_set_expander(
      EXPORTING
        node_key       = mv_node_key
        expander       = value
      EXCEPTIONS
        node_not_found = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_uitb_tree_error.
    ENDIF.

  ENDMETHOD.


  METHOD set_folder.
    mr_model->node_set_is_folder(
      EXPORTING
        node_key       = mv_node_key
        is_folder      = value
      EXCEPTIONS
        node_not_found = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_uitb_tree_error.
    ENDIF.

  ENDMETHOD.


  METHOD set_image.
    mr_model->node_set_image(
      EXPORTING
        node_key       = mv_node_key
        image          = value
      EXCEPTIONS
        node_not_found = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_uitb_tree_error.
    ENDIF.

  ENDMETHOD.


  METHOD set_style.
    mr_model->node_set_style(
      EXPORTING
        node_key       = mv_node_key
        style          = iv_style
      EXCEPTIONS
        node_not_found = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_uitb_tree_error.
    ENDIF.
  ENDMETHOD.


  METHOD set_user_object.
    mr_model->node_set_user_object(
      EXPORTING
        node_key       = mv_node_key
        user_object    = value
      EXCEPTIONS
        node_not_found = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_uitb_tree_error.
    ENDIF.

  ENDMETHOD.


  METHOD toggle.
    CHECK is_folder( ).

    DATA(lt_expanded) = mr_nodes->get_expanded_nodes( ).

    IF  line_exists( lt_expanded[ table_line = mv_node_key ] ).
      mr_nodes->collapse_node( iv_node_key = mv_node_key ).
    ELSE.
      mr_nodes->expand_node( iv_node_key = mv_node_key ).
    ENDIF.
  ENDMETHOD.

  METHOD get_user_data.
    IF mr_user_data IS NOT BOUND.
      mr_user_data = VALUE #( mr_nodes->mt_node_data[ node_key = mv_node_key ]-data OPTIONAL ).
    ENDIF.
    result = mr_user_data.
  ENDMETHOD.

  METHOD set_user_data.
    mr_nodes->update_node_user_data(
      iv_node_key  = mv_node_key
      ir_user_data = value ).
    mr_user_data = value.
  ENDMETHOD.

ENDCLASS.
