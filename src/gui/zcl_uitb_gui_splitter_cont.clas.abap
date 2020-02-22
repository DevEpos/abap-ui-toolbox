"! <p class="shorttext synchronized" lang="en">Wrapper for CL_GUI_SPLITTER_CONTAINER</p>
CLASS zcl_uitb_gui_splitter_cont DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_uitb_gui_control.

    CONSTANTS c_default_size TYPE string VALUE '50:50'.
    CONSTANTS c_size_separator TYPE char1 VALUE ':'.

    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Mode for splitter container</p>
      BEGIN OF c_mode,
        rows TYPE i VALUE 0,
        cols TYPE i VALUE 1,
      END OF c_mode.

    "! <p class="shorttext synchronized" lang="en">Creates new GUI Splitter Container</p>
    "!
    METHODS constructor
      IMPORTING
        iv_elements TYPE i
        iv_size     TYPE string DEFAULT c_default_size
        iv_mode     TYPE i DEFAULT zcl_uitb_gui_splitter_cont=>c_mode-rows
        io_parent   TYPE REF TO cl_gui_container.

    "! <p class="shorttext synchronized" lang="en">Set size of a given Cell</p>
    METHODS set_size
      IMPORTING
        iv_index TYPE i
        iv_size  TYPE i.
    "! <p class="shorttext synchronized" lang="en">Retrieve container at specific index</p>
    "!
    METHODS get_container
      IMPORTING
        iv_index            TYPE i
      RETURNING
        VALUE(ro_container) TYPE REF TO cl_gui_container.
    "! <p class="shorttext synchronized" lang="en">Sets the sash properties for a given element</p>
    "!
    METHODS set_sash_properties
      IMPORTING
        iv_index   TYPE i
        if_visible TYPE abap_bool OPTIONAL
        if_movable TYPE abap_bool OPTIONAL.

    "! <p class="shorttext synchronized" lang="en">Show/hide element</p>
    "!
    METHODS set_element_visibility
      IMPORTING
        iv_element TYPE i
        if_visible TYPE abap_bool OPTIONAL.

    "! <p class="shorttext synchronized" lang="en">Returns true if the element at given index is visible</p>
    METHODS is_element_visible
      IMPORTING
        iv_element           TYPE i
      RETURNING
        VALUE(rf_is_visible) TYPE abap_bool.

    "! <p class="shorttext synchronized" lang="en">Toggles visibility of the element at the given position</p>
    "!
    "! @parameter iv_element | <p class="shorttext synchronized" lang="en">the positional number of an element in the splitter</p>
    METHODS toggle_visibility
      IMPORTING
        iv_element TYPE i.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS c_true TYPE i VALUE cl_gui_splitter_container=>true.
    CONSTANTS c_false TYPE i VALUE cl_gui_splitter_container=>false.

    TYPES:
      BEGIN OF ty_s_element,
        base_size    TYPE i,
        size         TYPE i,
        visible      TYPE abap_bool,
        sash_visible TYPE abap_bool,
      END OF ty_s_element.

    DATA mt_elements TYPE STANDARD TABLE OF ty_s_element WITH EMPTY KEY.
    DATA mo_splitter TYPE REF TO cl_gui_splitter_container.
    DATA mv_mode TYPE i.
    DATA mv_width TYPE i.
    DATA mv_elements_count TYPE i.
    DATA mv_size_mode TYPE i.

    "! <p class="shorttext synchronized" lang="en">Update cell sizes</p>
    "!
    METHODS update_size
      IMPORTING
        if_initial_update TYPE abap_bool OPTIONAL.

    METHODS set_size_hidden_cells.
    METHODS set_size_visible_cells.
ENDCLASS.



CLASS zcl_uitb_gui_splitter_cont IMPLEMENTATION.
  METHOD constructor.
    DATA: lv_rows  TYPE i,
          lv_size  TYPE i,
          lv_total TYPE i,
          lt_size  TYPE STANDARD TABLE OF string,
          lv_cols  TYPE i.

    SPLIT iv_size AT c_size_separator INTO TABLE lt_size.

    IF iv_mode = c_mode-cols.
      lv_rows = 1.
      lv_cols = iv_elements.
    ELSEIF iv_mode = c_mode-rows.
      lv_cols = 1.
      lv_rows = iv_elements.
    ELSE.
      RAISE EXCEPTION TYPE zcx_uitb_gui_exception.
    ENDIF.

    mv_mode = iv_mode.

    mv_size_mode = cl_gui_splitter_container=>mode_relative.

*.. Determine elements with sizes
    LOOP AT lt_size INTO DATA(lv_size_string).
      DATA(lv_tabix) = sy-tabix.
      IF lv_size_string = '*'.
        mv_size_mode = cl_gui_splitter_container=>mode_absolute.
        lv_size = 0.
      ELSE.
        TRY.
            lv_size = CONV #( lv_size_string ).
          CATCH cx_sy_conversion_no_number.
            RAISE EXCEPTION TYPE zcx_uitb_gui_exception.
        ENDTRY.
*        lv_total = lv_total + lv_size.
      ENDIF.

      mt_elements = VALUE #(
        BASE mt_elements
        ( base_size    = lv_size
          visible      = abap_true
          sash_visible = abap_true )
      ).

    ENDLOOP.

    mv_elements_count = lines( mt_elements ).

*.. Create the splitter control
    CREATE OBJECT mo_splitter
      EXPORTING
        parent                  = io_parent
        rows                    = lv_rows
        columns                 = lv_cols
        no_autodef_progid_dynnr = abap_true
      EXCEPTIONS
        cntl_error              = 1
        cntl_system_error       = 2
        OTHERS                  = 3.
    IF sy-subrc <> 0.
      zcx_uitb_gui_exception=>raise_from_sy( ).
    ENDIF.

    CASE mv_mode.

      WHEN c_mode-cols.
        mo_splitter->set_column_mode(
          EXPORTING
            mode              = mv_size_mode
          EXCEPTIONS
            cntl_error        = 1
            cntl_system_error = 2
            OTHERS            = 3
        ).
        IF sy-subrc <> 0.
          zcx_uitb_gui_exception=>raise_from_sy( ).
        ENDIF.

      WHEN c_mode-rows.
        mo_splitter->set_row_mode(
          EXPORTING
            mode              = mv_size_mode
          EXCEPTIONS
            cntl_error        = 1
            cntl_system_error = 2
            OTHERS            = 3
        ).
        IF sy-subrc <> 0.
          zcx_uitb_gui_exception=>raise_from_sy( ).
        ENDIF.
    ENDCASE.

*.. Hide the border
    mo_splitter->set_border(
      EXPORTING
        border            = abap_false
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3
    ).
    IF sy-subrc NE 0.
      zcx_uitb_gui_exception=>raise_from_sy( ).
    ENDIF.

    update_size( if_initial_update = abap_true ).
  ENDMETHOD.

  METHOD get_container.
    ro_container = mo_splitter->get_container(
      row    = COND #( WHEN mv_mode = c_mode-cols THEN 1 ELSE iv_index )
      column = COND #( WHEN mv_mode = c_mode-rows THEN 1 ELSE iv_index )
    ).
  ENDMETHOD.

  METHOD set_sash_properties.
    DATA(lv_movable) = COND i( WHEN if_movable = abap_true THEN c_true ELSE c_false ).
    DATA(lv_visible) = COND i( WHEN if_visible = abap_true THEN c_true ELSE c_false ).

    CASE mv_mode.

      WHEN c_mode-cols.
        IF if_movable IS SUPPLIED.
          mo_splitter->set_column_sash(
            EXPORTING
              id                = 1
              type              = cl_gui_splitter_container=>type_movable
              value             = lv_movable
            EXCEPTIONS
              cntl_error        = 1
              cntl_system_error = 2
              OTHERS            = 3
          ).
          IF sy-subrc NE 0.
            zcx_uitb_gui_exception=>raise_from_sy( ).
          ENDIF.
        ENDIF.
        IF if_visible IS SUPPLIED.
          mo_splitter->set_column_sash(
            EXPORTING
              id                = 1
              type              = cl_gui_splitter_container=>type_sashvisible
              value             = lv_visible
            EXCEPTIONS
              cntl_error        = 1
              cntl_system_error = 2
              OTHERS            = 3
          ).
          IF sy-subrc NE 0.
            zcx_uitb_gui_exception=>raise_from_sy( ).
          ENDIF.
        ENDIF.

      WHEN c_mode-rows.
        IF if_movable IS SUPPLIED.
          mo_splitter->set_row_sash(
            EXPORTING
              id                = 1
              type              = cl_gui_splitter_container=>type_movable
              value             = lv_movable
            EXCEPTIONS
              cntl_error        = 1
              cntl_system_error = 2
              OTHERS            = 3
          ).
          IF sy-subrc NE 0.
            zcx_uitb_gui_exception=>raise_from_sy( ).
          ENDIF.
        ENDIF.
        IF if_visible IS SUPPLIED.
          mo_splitter->set_row_sash(
            EXPORTING
              id                = 1
              type              = cl_gui_splitter_container=>type_sashvisible
              value             = lv_visible
            EXCEPTIONS
              cntl_error        = 1
              cntl_system_error = 2
              OTHERS            = 3
          ).
          IF sy-subrc NE 0.
            zcx_uitb_gui_exception=>raise_from_sy( ).
          ENDIF.
        ENDIF.
    ENDCASE.
  ENDMETHOD.

  METHOD set_size.
    CASE mv_mode.

      WHEN c_mode-cols.
        mo_splitter->set_column_width(
          EXPORTING
            id                = iv_index
            width             = iv_size
          EXCEPTIONS
            cntl_error        = 1
            cntl_system_error = 2
            OTHERS            = 3
        ).
        IF sy-subrc <> 0.
          zcx_uitb_gui_exception=>raise_from_sy( ).
        ENDIF.

      WHEN c_mode-rows.
        mo_splitter->set_row_height(
          EXPORTING
            id                = iv_index
            height            = iv_size
          EXCEPTIONS
            cntl_error        = 1
            cntl_system_error = 2
            OTHERS            = 3
        ).
        IF sy-subrc <> 0.
          zcx_uitb_gui_exception=>raise_from_sy( ).
        ENDIF.
    ENDCASE.

*.. Update size property for element
    DATA(lr_element) = REF #( mt_elements[ iv_index ] ).
    lr_element->size = iv_size.
  ENDMETHOD.

  METHOD set_element_visibility.
    DATA: lv_count TYPE i,
          lf_delta TYPE abap_bool,
          lf_show  TYPE abap_bool.

    FIELD-SYMBOLS: <ls_element> LIKE LINE OF mt_elements.

    DATA(lr_element) = REF #( mt_elements[ iv_element ] OPTIONAL ).
    CHECK lr_element IS BOUND.

    IF if_visible IS NOT SUPPLIED.
      lf_show = lr_element->visible.
      zcl_uitb_appl_util=>toggle( CHANGING value = lf_show ).
    ELSE.
      lf_show = if_visible.
    ENDIF.

*.. check child visibility
    IF lr_element->visible <> lf_show.

*.... visibility delta
      lr_element->visible = lf_show.
      lf_delta = abap_true.

      LOOP AT mt_elements ASSIGNING <ls_element> WHERE visible = abap_true.
        lv_count = lv_count + 1.
      ENDLOOP.

      LOOP AT mt_elements ASSIGNING <ls_element>.
        IF <ls_element>-visible = abap_true.
          lv_count = lv_count - 1.
          IF lv_count > 0.
            <ls_element>-sash_visible = abap_true.
          ELSE.
            <ls_element>-sash_visible = abap_false.
          ENDIF.

        ELSE.
          <ls_element>-sash_visible = abap_false.
        ENDIF.
      ENDLOOP.
    ELSE.
      lf_delta = abap_false.
    ENDIF.

    IF lf_delta = abap_true.
      update_size( ).
    ENDIF.

  ENDMETHOD.

  METHOD toggle_visibility.
    set_element_visibility( iv_element = iv_element if_visible = xsdbool( NOT is_element_visible( iv_element ) ) ).
  ENDMETHOD.

  METHOD is_element_visible.
    rf_is_visible = VALUE #( mt_elements[ iv_element ]-visible OPTIONAL ).
  ENDMETHOD.

  METHOD update_size.
    mo_splitter->get_width( IMPORTING width = mv_width ).
    set_size_visible_cells( ).
    set_size_hidden_cells( ).
  ENDMETHOD.

  METHOD set_size_visible_cells.
    DATA: lv_tabix TYPE i.

    FIELD-SYMBOLS: <ls_element> LIKE LINE OF mt_elements.

    LOOP AT mt_elements ASSIGNING <ls_element> WHERE visible = abap_true.
      lv_tabix = sy-tabix.

      CHECK ( mv_size_mode = cl_gui_splitter_container=>mode_relative OR
              ( mv_size_mode = cl_gui_splitter_container=>mode_absolute AND lv_tabix < mv_elements_count ) ).

      set_size(
          iv_index = lv_tabix
          iv_size  = <ls_element>-base_size
      ).

      CHECK lv_tabix < mv_elements_count.

      set_sash_properties(
          iv_index   = lv_tabix
          if_visible = <ls_element>-sash_visible
      ).
    ENDLOOP.
  ENDMETHOD.

  METHOD set_size_hidden_cells.
    LOOP AT mt_elements ASSIGNING FIELD-SYMBOL(<ls_element>) WHERE visible = abap_false.
      DATA(lv_tabix) = sy-tabix.
      set_size(
          iv_index = lv_tabix
          iv_size  = 0
      ).
      CHECK lv_tabix < mv_elements_count.

      set_sash_properties(
          iv_index   = lv_tabix
          if_visible = <ls_element>-sash_visible
      ).

    ENDLOOP.
  ENDMETHOD.

  METHOD zif_uitb_gui_control~free.
    CHECK mo_splitter IS BOUND.
    mo_splitter->free( EXCEPTIONS OTHERS = 1 ).
  ENDMETHOD.

  METHOD zif_uitb_gui_control~focus.
    CHECK mo_splitter IS BOUND.

    cl_gui_control=>set_focus( mo_splitter ).
  ENDMETHOD.

  METHOD zif_uitb_gui_control~has_focus.
    CHECK mo_splitter IS BOUND.
    cl_gui_control=>get_focus( IMPORTING control = zif_uitb_gui_control~mr_control ).

    rf_has_focus = xsdbool( zif_uitb_gui_control~mr_control = mo_splitter ).
  ENDMETHOD.

ENDCLASS.
