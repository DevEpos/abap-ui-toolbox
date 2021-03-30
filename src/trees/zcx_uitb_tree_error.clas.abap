"! <p class="shorttext synchronized" lang="en">Tree Error</p>
CLASS zcx_uitb_tree_error DEFINITION
  PUBLIC
  INHERITING FROM zcx_uitb_application_exc
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF zcx_uitb_tree_error,
        msgid TYPE symsgid VALUE 'ZUITB_EXCEPTION',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_uitb_tree_error.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_uitb_tree_error IMPLEMENTATION.

ENDCLASS.
