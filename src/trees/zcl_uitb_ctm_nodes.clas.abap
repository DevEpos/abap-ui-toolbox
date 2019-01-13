"! <p class="shorttext synchronized" lang="en">Nodes of Column Tree Model</p>
CLASS zcl_uitb_ctm_nodes DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">Adds a new node to the model</p>
    METHODS add_node
      IMPORTING
        !iv_node_key          TYPE tm_nodekey OPTIONAL
        !iv_relative_node_key TYPE tm_nodekey OPTIONAL
        !iv_relationship      TYPE i DEFAULT cl_tree_model=>relat_last_child
        !iv_drag_drop_id      TYPE i OPTIONAL
        !if_folder            TYPE as4flag OPTIONAL
        !if_hidden            TYPE as4flag OPTIONAL
        !if_disabled          TYPE as4flag OPTIONAL
        !iv_style             TYPE int4 OPTIONAL
        !if_no_branch         TYPE as4flag OPTIONAL
        !if_expander          TYPE as4flag OPTIONAL
        !iv_image             TYPE tv_image OPTIONAL
        !iv_expanded_image    TYPE tv_image OPTIONAL
        !ir_user_object       TYPE REF TO object OPTIONAL
        !if_items_incomplete  TYPE as4flag OPTIONAL
        !it_item_table        TYPE treemcitab
      RETURNING
        VALUE(rr_node)        TYPE REF TO zcl_uitb_ctm_node
      RAISING
        zcx_uitb_tree_error .
    "! <p class="shorttext synchronized" lang="en">Adds multiple new nodes to the model</p>
    METHODS add_nodes
      IMPORTING
        !it_node_table TYPE treemcnota
        !it_item_table TYPE treemcitac
      RAISING
        zcx_uitb_tree_error .
    "! <p class="shorttext synchronized" lang="en">Collapses all nodes</p>
    METHODS collapse_all_nodes .
    "! <p class="shorttext synchronized" lang="en">Collapses a single node</p>
    METHODS collapse_node
      IMPORTING
        !iv_node_key         TYPE tm_nodekey
        !if_collapse_subtree TYPE abap_bool OPTIONAL .
    METHODS constructor
      IMPORTING
        !ir_model         TYPE REF TO cl_column_tree_model
        !if_auto_node_key TYPE abap_bool OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Deletes all nodes from the model</p>
    METHODS delete_all_nodes .
    "! <p class="shorttext synchronized" lang="en">Delete a single node from the model</p>
    METHODS delete_node
      IMPORTING
        !iv_node_key TYPE tm_nodekey
      RAISING
        zcx_uitb_tree_error .
    "! <p class="shorttext synchronized" lang="en">Deletes multiple nodes from the model</p>
    METHODS delete_nodes
      IMPORTING
        !it_node_key_table TYPE treemnotab
      RAISING
        zcx_uitb_tree_error .
    "! <p class="shorttext synchronized" lang="en">Expands a single node</p>
    METHODS expand_node
      IMPORTING
        !iv_node_key            TYPE tm_nodekey
        !if_expand_predecessors TYPE abap_bool OPTIONAL
        !if_expand_subtree      TYPE abap_bool OPTIONAL
        !iv_level_count         TYPE i OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Expand given nodes</p>
    METHODS expand_nodes
      IMPORTING
        !it_node_key TYPE treemnotab .
    "! <p class="shorttext synchronized" lang="en">Expands root nodes</p>
    METHODS expand_root_nodes
      IMPORTING
        !if_expand_subtree TYPE abap_bool DEFAULT abap_true
        !iv_level_count    TYPE i OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Retrieve all nodes that are expanded</p>
    METHODS get_expanded_nodes
      IMPORTING
        !if_no_hidden_nodes TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(result)       TYPE treemnotab .
    "! <p class="shorttext synchronized" lang="en">Gets a specific node for a key</p>
    METHODS get_node
      IMPORTING
        !iv_node_key   TYPE tm_nodekey
      RETURNING
        VALUE(rr_node) TYPE REF TO zcl_uitb_ctm_node
      RAISING
        zcx_uitb_tree_error .
    "! <p class="shorttext synchronized" lang="en">Retrieves references of the root nodes</p>
    "!
    "! @parameter rt_root_nodes | <p class="shorttext synchronized" lang="en">References List of the root nodes</p>
    METHODS get_root_nodes
      RETURNING
        VALUE(rt_root_nodes) TYPE zuitb_ctm_node_rt .
    "! <p class="shorttext synchronized" lang="en">Retrieves keys of the root nodes</p>
    "!
    "! @parameter rt_root_node_keys | <p class="shorttext synchronized" lang="en">List of keys of the root nodes</p>
    METHODS get_root_node_keys
      RETURNING
        VALUE(rt_root_node_keys) TYPE treemnotab .
    "! <p class="shorttext synchronized" lang="en">Gets the current top node</p>
    METHODS get_top_node
      RETURNING
        VALUE(rr_node) TYPE REF TO zcl_uitb_ctm_node
      RAISING
        zcx_uitb_tree_error .
    "! <p class="shorttext synchronized" lang="en">Sets the first root node as the new top node</p>
    METHODS set_first_root_node_as_top .
    "! <p class="shorttext synchronized" lang="en">Set the top node to be selected and in the view box</p>
    METHODS set_top_node
      IMPORTING
        !iv_node_key TYPE tm_nodekey .
    "! <p class="shorttext synchronized" lang="en">Toggles expanded state of given nodes</p>
    METHODS toggle_node
      IMPORTING
        !iv_node_key TYPE tm_nodekey .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mr_model TYPE REF TO cl_column_tree_model .
    DATA mf_auto_node_key TYPE abap_bool .
    CLASS-DATA mv_last_node_key TYPE i .

    METHODS get_next_node_key
      RETURNING
        VALUE(rv_new_node_key) TYPE tm_nodekey .
ENDCLASS.



CLASS zcl_uitb_ctm_nodes IMPLEMENTATION.


  METHOD add_node.
    DATA(lv_node_key) = iv_node_key.
    IF lv_node_key IS INITIAL.
      ASSERT mf_auto_node_key = abap_true.

*.... Create node key
      lv_node_key = get_next_node_key( ).
    ENDIF.

    mr_model->add_node(
      EXPORTING
        node_key                = lv_node_key             " Schlüssel des Knotens
        relative_node_key       = iv_relative_node_key    " Schlüssel des verwandten Knotens
        relationship            = iv_relationship         " Verwandschaftsbeziehung
        drag_drop_id            = iv_drag_drop_id
        isfolder                = if_folder             " 'X': Knoten ist Ordner; ' ': Knoten ist Blatt
        hidden                  = if_hidden               " 'X': Knoten ist unsichtbar
        disabled                = if_disabled             " 'X': Knoten kann nicht selektiert werden
        style                   = iv_style                " siehe Methodendokumentation
        no_branch               = if_no_branch            " 'X': Keine Hierarchielinie zeichnen
        expander                = if_expander             " siehe Methodendokumentation
        image                   = iv_image                " siehe Methodendokumentation
        expanded_image          = iv_expanded_image       " siehe Methodendokumentation
        user_object             = ir_user_object          " User Objekt
        items_incomplete        = if_items_incomplete     " siehe Methodendokumentation
        item_table              = it_item_table           " Items des Knotens
      EXCEPTIONS
        node_key_exists         = 1
        node_key_empty          = 2
        illegal_relationship    = 3
        relative_node_not_found = 4
        error_in_item_table     = 5
        OTHERS                  = 6
    ).
    IF sy-subrc <> 0.
      zcx_uitb_tree_error=>raise_from_sy( ).
    ELSE.
      rr_node = get_node( lv_node_key ).
    ENDIF.
  ENDMETHOD.


  METHOD add_nodes.
    mr_model->add_nodes(
      EXPORTING
        node_table          = it_node_table
      EXCEPTIONS
        error_in_node_table = 1
        OTHERS              = 2
    ).
    IF sy-subrc <> 0.
      zcx_uitb_tree_error=>raise_from_sy( ).
    ENDIF.

    mr_model->add_items(
      EXPORTING
        item_table          = it_item_table
      EXCEPTIONS
        node_not_found      = 1
        error_in_item_table = 2
        OTHERS              = 3
    ).
    IF sy-subrc <> 0.
      zcx_uitb_tree_error=>raise_from_sy( ).
    ENDIF.
  ENDMETHOD.


  METHOD collapse_all_nodes.
    mr_model->collapse_all_nodes( ).
  ENDMETHOD.


  METHOD collapse_node.
    mr_model->collapse_node(
        node_key         = iv_node_key
        collapse_subtree = if_collapse_subtree
    ).
  ENDMETHOD.


  METHOD constructor.
    mf_auto_node_key = if_auto_node_key.
    mr_model = ir_model.
  ENDMETHOD.


  METHOD delete_all_nodes.
    mr_model->delete_all_nodes( ).
  ENDMETHOD.


  METHOD delete_node.
    mr_model->delete_node(
      EXPORTING
        node_key       = iv_node_key    " Schlüssel des Knotens
      EXCEPTIONS
        node_not_found = 1
        OTHERS         = 2
    ).
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD delete_nodes.
    mr_model->delete_nodes(
      EXPORTING
        node_key_table          = it_node_key_table    " Tabelle mit Knotenschlüsseln
      EXCEPTIONS
        error_in_node_key_table = 1
        OTHERS                  = 2
    ).
    IF sy-subrc <> 0.
      zcx_uitb_tree_error=>raise_from_sy( ).
    ENDIF.
  ENDMETHOD.


  METHOD expand_node.
    mr_model->expand_node(
        node_key            = iv_node_key    " Schlüssel des Knotens
        expand_predecessors = if_expand_predecessors    " 'X': Vorgänger des Knotens expandieren
        expand_subtree      = if_expand_subtree    " 'X': alle Nachfahren expandieren
        level_count         = iv_level_count    " Anzahl zu expandierende Nachfolgeebenen
    ).
  ENDMETHOD.


  METHOD expand_nodes.
    mr_model->expand_nodes(
        node_key_table = it_node_key
    ).
  ENDMETHOD.


  METHOD expand_root_nodes.
    mr_model->expand_root_nodes(
        expand_subtree      = if_expand_subtree
        level_count         = iv_level_count
    ).
  ENDMETHOD.


  METHOD get_expanded_nodes.
    mr_model->get_expanded_nodes(
      EXPORTING no_hidden_nodes = if_no_hidden_nodes
      IMPORTING node_key_table  = result
    ).
  ENDMETHOD.


  METHOD get_next_node_key.
    IF mv_last_node_key IS INITIAL.
      mv_last_node_key = 1.
    ELSE.
      ADD 1 TO mv_last_node_key.
    ENDIF.

    rv_new_node_key = mv_last_node_key.

*.. Check if the nodekey is still free
    WHILE mr_model->node_key_in_tree( rv_new_node_key ).
      ADD 1 TO mv_last_node_key.
      rv_new_node_key = mv_last_node_key.
    ENDWHILE.
  ENDMETHOD.


  METHOD get_node.
    mr_model->node_get_properties(
      EXPORTING
        node_key       = iv_node_key
      EXCEPTIONS
        node_not_found = 1
        OTHERS         = 2
    ).
    IF sy-subrc <> 0.
      zcx_uitb_tree_error=>raise_from_sy( ).
    ELSE.
      rr_node = NEW #(
        ir_model    = mr_model
        iv_node_key = iv_node_key
        ir_nodes    = me
      ).
    ENDIF.

  ENDMETHOD.


  METHOD get_root_nodes.
    TRY.
        rt_root_nodes = VALUE #( FOR node_key IN get_root_node_keys( ) ( get_node( node_key ) ) ).
      CATCH zcx_uitb_tree_error ##needed.
        "handle exception
    ENDTRY.
  ENDMETHOD.


  METHOD get_root_node_keys.
    mr_model->get_root_nodes( IMPORTING node_key_table = rt_root_node_keys ).
  ENDMETHOD.


  METHOD get_top_node.
    mr_model->get_top_node(
      IMPORTING
        node_key             = DATA(lv_top_node)    " Node key
      EXCEPTIONS
        control_not_existing = 1
        control_dead         = 2
        cntl_system_error    = 3
        failed               = 4
        OTHERS               = 5
    ).
    IF sy-subrc <> 0.
      zcx_uitb_tree_error=>raise_from_sy( ).
    ELSE.
      CHECK lv_top_node IS NOT INITIAL.
      rr_node = get_node( lv_top_node ).
    ENDIF.
  ENDMETHOD.


  METHOD set_first_root_node_as_top.
    DATA(lv_first_root_node) = mr_model->get_first_root_node( ).

    CHECK lv_first_root_node IS NOT INITIAL.

    set_top_node( iv_node_key = lv_first_root_node ).
  ENDMETHOD.


  METHOD set_top_node.
    mr_model->set_top_node( iv_node_key ).
  ENDMETHOD.


  METHOD toggle_node.
    DATA(lt_expanded_nodes) = get_expanded_nodes( ).
    IF line_exists( lt_expanded_nodes[ table_line = iv_node_key ] ).
      collapse_node( iv_node_key ).
    ELSE.
      expand_node( iv_node_key ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
