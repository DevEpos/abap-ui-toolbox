"! <p class="shorttext synchronized" lang="en">Events for Tree Model</p>
INTERFACE zif_uitb_tree_model_events
  PUBLIC .


  "! <p class="shorttext synchronized" lang="en">A drag with a single node was started</p>
  EVENTS drag
    EXPORTING
      VALUE(ev_node_key) TYPE tm_nodekey
      VALUE(ev_item_name) TYPE tv_itmname
      VALUE(er_drag_drop_object) TYPE REF TO cl_dragdropobject .
  "! <p class="shorttext synchronized" lang="en">A drag with multiple nodes was started</p>
  EVENTS drag_multiple
    EXPORTING
      VALUE(et_node_key_table) TYPE treemnotab
      VALUE(ev_item_name) TYPE tv_itmname
      VALUE(er_drag_drop_object) TYPE REF TO cl_dragdropobject .
  "! <p class="shorttext synchronized" lang="en">A single node drop has been completed</p>
  EVENTS drop_complete
    EXPORTING
      VALUE(ev_node_key) TYPE tm_nodekey
      VALUE(ev_item_name) TYPE tv_itmname
      VALUE(er_drag_drop_object) TYPE REF TO cl_dragdropobject .
  "! <p class="shorttext synchronized" lang="en">A drop has been done</p>
  EVENTS drop
    EXPORTING
      VALUE(ev_node_key) TYPE tm_nodekey
      VALUE(er_drag_drop_object) TYPE REF TO cl_dragdropobject .
  "! <p class="shorttext synchronized" lang="en">A drop with multiple nodes was completed</p>
  EVENTS drop_complete_multiple
    EXPORTING
      VALUE(et_node_key_table) TYPE treemnotab
      VALUE(ev_item_name) TYPE tv_itmname
      VALUE(er_drag_drop_object) TYPE REF TO cl_dragdropobject .
  "! <p class="shorttext synchronized" lang="en">Context menu was requested for node</p>
  EVENTS node_context_menu_request
    EXPORTING
      VALUE(ev_node_key) TYPE tm_nodekey
      VALUE(er_menu) TYPE REF TO cl_ctmenu .
  "! <p class="shorttext synchronized" lang="en">Context menu entry was selected</p>
  EVENTS node_context_menu_select
    EXPORTING
      VALUE(ev_node_key) TYPE tm_nodekey
      VALUE(ev_fcode) TYPE sy-ucomm .
  "! <p class="shorttext synchronized" lang="en">Node was double clicked</p>
  EVENTS node_double_click
    EXPORTING
      VALUE(ev_node_key) TYPE tm_nodekey .
  "! <p class="shorttext synchronized" lang="en">Keypress on Node detected</p>
  EVENTS node_keypress
    EXPORTING
      VALUE(ev_node_key) TYPE tm_nodekey
      VALUE(ev_key) TYPE i .
  "! <p class="shorttext synchronized" lang="en">Selection has changed</p>
  EVENTS selection_changed
    EXPORTING
      VALUE(ev_node_key) TYPE tm_nodekey .
  "! <p class="shorttext synchronized" lang="en">Expander was opened</p>
  EVENTS expand_no_children
    EXPORTING
      VALUE(ev_node_key) TYPE tm_nodekey .
  "! <p class="shorttext synchronized" lang="en">Button was clicked ( Item Event)</p>
  EVENTS button_click
    EXPORTING
      VALUE(ev_node_key) TYPE tm_nodekey
      VALUE(ev_item_name) TYPE tv_itmname .
  "! <p class="shorttext synchronized" lang="en">Link was clicked (Item event)</p>
  EVENTS link_click
    EXPORTING
      VALUE(ev_node_key) TYPE tm_nodekey
      VALUE(ev_item_name) TYPE tv_itmname .
  "! <p class="shorttext synchronized" lang="en">Item was double clicked</p>
  EVENTS item_double_click
    EXPORTING
      VALUE(ev_node_key) TYPE tm_nodekey
      VALUE(ev_item_name) TYPE tv_itmname .
  "! <p class="shorttext synchronized" lang="en">Keypress on Item detected (Item Event)</p>
  EVENTS item_keypress
    EXPORTING
      VALUE(ev_node_key) TYPE tm_nodekey
      VALUE(ev_key) TYPE i
      VALUE(ev_item_name) TYPE tv_itmname .
  "! <p class="shorttext synchronized" lang="en">Checkbox state has changed (Item Event )</p>
  EVENTS checkbox_change
    EXPORTING
      VALUE(ev_node_key) TYPE tm_nodekey
      VALUE(ev_item_name) TYPE tv_itmname
      VALUE(ef_checked) TYPE abap_bool .
ENDINTERFACE.
