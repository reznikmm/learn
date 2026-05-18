.. include:: ../../../global.txt

Static Expression Functions
===========================

.. note::

    Static expression functions are supported by

    * GNAT Community Edition 2021
    * GCC 11

Ada 2022 introduces the aspect :ada:`Static` for expression functions.
When this aspect is :ada:`True`, the function becomes a
`static expression function`, which means calls to it may appear in
contexts that require static expressions.

This is useful when a static value is not a simple literal, and you want
to give it a meaningful name instead of repeating a larger expression.

Basic Example
-------------

In this example, :ada:`Kilobytes` and :ada:`Megabytes` are expression
functions marked with :ada:`Static`. Their calls can be used where a
static value is required.

.. code:: ada run_button project=Courses.Ada_2022_Whats_New.Static_Expression_Functions switches=Compiler(-gnat2022);

   with Ada.Text_IO;

   procedure Main is

      function Kilobytes (N : Natural) return Natural is
        (N * 1024) with Static;

      function Megabytes (N : Natural) return Natural is
        (Kilobytes (N) * 1024) with Static;

      Buffer_Size : constant Natural := Kilobytes (4);

      subtype Allowed_Size is Natural range 0 .. Megabytes (2);
      Value : Allowed_Size := Buffer_Size;

   begin
      Ada.Text_IO.Put_Line (Value'Image);
   end Main;

All Actual Parameters Must Be Static
------------------------------------

When a call appears in a context that requires a static expression,
every actual parameter must also be static.

So, this call is rejected because :ada:`V` is a variable:

.. code-block:: ada

   function If_Then_Else
     (Flag : Boolean; X, Y : Integer) return Integer is
     (if Flag then X else Y) with Static;

   V : Integer := 10;
   X : constant Integer := If_Then_Else (True, 37, V); --  Error

Even though the condition is :ada:`True`, function call rules still apply
to all parameters in a static context.

Extensible Enumeration Pattern
------------------------------

Static expression functions are especially useful for protocol values that
are open for future extension.

With a classic Ada enumeration type, unknown values raise
:ada:`Constraint_Error` during conversion. For external protocols, this can
be too strict.

In Ada 2022, we can keep the underlying type numeric and declare known
values as static functions:

.. code-block:: ada

   with Interfaces;

   type Safety_Switch_State is new Interfaces.Unsigned_8;

   function Safe return Safety_Switch_State is (0) with Static;
   function Dangerous return Safety_Switch_State is (1) with Static;

   subtype Safety_Switch_State_Well_Known is Safety_Switch_State
     with Static_Predicate =>
       Safety_Switch_State_Well_Known in Safe .. Dangerous;

   function Well_Known_Image
     (Value : Safety_Switch_State_Well_Known) return String is
     (case Value is
        when Safe      => "Safe",
        when Dangerous => "Dangerous")
     with Static;

This gives a practical combination:

* known values are checked statically in :ada:`case` expressions;
* unknown numeric values can still be represented and handled at run time;
* names like :ada:`Safe` and :ada:`Dangerous` can be overloaded for other
  types, so long prefixes are not required.

References
----------

* :arm22:`ARM 6.8 Expression Functions <6-8>`
* :aarm22:`Overview 7.5 Static expression functions <Ov22-7-5>`
* AI12-0075-1_

 .. _AI12-0075-1: http://www.ada-auth.org/cgi-bin/cvsweb.cgi/AI12s/AI12-0075-1.TXT