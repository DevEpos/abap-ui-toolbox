FUNCTION-POOL zuitb_gui_template.          "MESSAGE-ID ..

INCLUDE lzuitb_gui_templated01.

CONSTANTS: gc_max_function_number TYPE i VALUE 28.

DATA ok_code TYPE sy-ucomm.

DATA: BEGIN OF gs_view_data,
        controller               TYPE REF TO zcl_uitb_templt_prog_callback,
        cached_container_content TYPE REF TO cl_gui_control,
        cached_controls          TYPE zcl_uitb_templt_prog_callback=>tt_controls,
        functions                TYPE zcl_uitb_templt_prog_callback=>tt_dynamic_functions,
        status_prog              TYPE sy-repid,
        status                   TYPE syst_pfkey,
        is_first_call            TYPE abap_bool,
        title                    TYPE syst_title,
        as_dialog                TYPE abap_bool,
      END OF gs_view_data.

DATA gt_view_controller LIKE STANDARD TABLE OF gs_view_data.

DATA: choose TYPE smp_dyntxt,
      func1  TYPE smp_dyntxt,
      func2  TYPE smp_dyntxt,
      func3  TYPE smp_dyntxt,
      func4  TYPE smp_dyntxt,
      func5  TYPE smp_dyntxt,
      func6  TYPE smp_dyntxt,
      func7  TYPE smp_dyntxt,
      func8  TYPE smp_dyntxt,
      func9  TYPE smp_dyntxt,
      func10 TYPE smp_dyntxt,
      func11 TYPE smp_dyntxt,
      func12 TYPE smp_dyntxt,
      func13 TYPE smp_dyntxt,
      func14 TYPE smp_dyntxt,
      func15 TYPE smp_dyntxt,
      func16 TYPE smp_dyntxt,
      func17 TYPE smp_dyntxt,
      func18 TYPE smp_dyntxt,
      func19 TYPE smp_dyntxt,
      func20 TYPE smp_dyntxt,
      func21 TYPE smp_dyntxt,
      func22 TYPE smp_dyntxt,
      func23 TYPE smp_dyntxt,
      func24 TYPE smp_dyntxt,
      func25 TYPE smp_dyntxt,
      func26 TYPE smp_dyntxt,
      func27 TYPE smp_dyntxt,
      func28 TYPE smp_dyntxt.
