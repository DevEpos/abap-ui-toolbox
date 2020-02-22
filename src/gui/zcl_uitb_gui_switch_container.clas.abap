CLASS zcl_uitb_gui_switch_container DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Creates new Switch container</p>
    "!
    METHODS constructor
      IMPORTING
        io_parent   TYPE REF TO cl_gui_container
        iv_lifetime TYPE i DEFAULT cntl_lifetime_dynpro.

    "! <p class="shorttext synchronized" lang="en">Adds new child to the switch and makes it visible</p>
    "!
    METHODS add_child
      IMPORTING
        iv_id               TYPE string
      RETURNING
        VALUE(ro_container) TYPE REF TO cl_gui_container.
    "! <p class="shorttext synchronized" lang="en">Removes the child with the given id</p>
    "!
    METHODS remove_child
      IMPORTING
        iv_id TYPE string.

    "! <p class="shorttext synchronized" lang="en">Sets the child with the given id visible/invisible</p>
    "!
    METHODS set_child_visible
      IMPORTING
        iv_id TYPE string
        if_visible type abap_bool default abap_true.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_s_child,
        id        TYPE string,
        container TYPE REF TO cl_gui_container,
        visible   TYPE abap_bool,
      END OF ty_s_child.
    DATA mo_parent TYPE REF TO cl_gui_container.
    DATA mv_lifetime TYPE i.
    DATA mt_children TYPE STANDARD TABLE OF ty_s_child WITH KEY id.
ENDCLASS.



CLASS zcl_uitb_gui_switch_container IMPLEMENTATION.
  METHOD add_child.
    IF line_exists( mt_children[ id = iv_id ] ).
      RAISE EXCEPTION TYPE zcx_uitb_gui_exception.
    ENDIF.

    ro_container = zcl_uitb_gui_helper=>create_control_container(
       io_parent   = mo_parent
       iv_lifetime = mv_lifetime
    ).

    ro_container->set_visible( abap_true ).

    LOOP AT mt_children ASSIGNING FIELD-SYMBOL(<ls_control>) WHERE visible = abap_true.
      <ls_control>-container->set_visible( abap_false ).
    ENDLOOP.

    mt_children = VALUE #(
      BASE mt_children
      ( id        = iv_id
        container = ro_container
        visible   = abap_true )
    ).
  ENDMETHOD.

  METHOD constructor.
    mo_parent = io_parent.
    mv_lifetime = iv_lifetime.
  ENDMETHOD.

  METHOD remove_child.
    LOOP AT mt_children ASSIGNING FIELD-SYMBOL(<ls_child>) WHERE id = iv_id.
      <ls_child>-container->free( EXCEPTIONS OTHERS = 1 ).
      DELETE mt_children.
      EXIT.
    ENDLOOP.

  ENDMETHOD.

  METHOD set_child_visible.
    DATA(lr_child) = REF #( mt_children[ id = iv_id ] OPTIONAL ).
    CHECK: lr_child IS BOUND,
           lr_child->container IS BOUND,
           lr_child->visible <> if_visible.

    lr_child->visible = if_visible.
    lr_child->container->set_visible( EXPORTING visible = if_visible EXCEPTIONS OTHERS = 1 ).
  ENDMETHOD.

ENDCLASS.
