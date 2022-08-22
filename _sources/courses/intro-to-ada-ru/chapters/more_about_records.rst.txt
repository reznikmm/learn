Подробнее о записях
===================

.. include:: ../../global.txt

Типы записей динамически изменяемого размера
--------------------------------------------

Ранее мы видели
:ref:`несколько простых примеров типов записей <Intro_Ada_Ru_Record_Type_Declaration>`.
Давайте рассмотрим некоторые из более продвинутых возможностей этой фундаментальной
конструкции языка Ада.

Следует отметить, что размер объекта для типа записи не обязательно
должен быть известен во время компиляции. Это проиллюстрировано в
приведенном ниже примере:

.. code:: ada no_button project=Courses.Intro_To_Ada.More_About_Records.Var_Size_Record
    :class: ada-syntax-only

    package Runtime_Length is
       function Compute_Max_Len return Natural;
    end Runtime_Length;

    with Runtime_Length; use Runtime_Length;

    package Var_Size_Record is
        Max_Len : constant Natural
          := Compute_Max_Len;
        --   ^ Not known at compile time

        type Items_Array is array (Positive range <>)
          of Integer;

        type Growable_Stack is record
           Items : Items_Array (1 .. Max_Len);
           Len   : Natural;
        end record;
        --  Growable_Stack is a definite type, but
        --  size is not known at compile time.

        G : Growable_Stack;
    end Var_Size_Record;

Совершенно нормально определять размер ваших записей во время
выполнения, но учтите, что все объекты этого типа будут иметь
одинаковый размер.

Записи с дискриминантом
-----------------------

В приведенном выше примере размер поля Items определяется один раз во
время выполнения, но размер всех экземпляров :ada:`Growable_Stack` будет
совпадать.
И, возможно, это не то, что вы хотите получить. Мы видели, что массивы
в целом имеют такую гибкость: для неограниченного типа массива
разные объекты могут иметь разные размеры.

Получить аналогичную функциональность для записей можно
используя специальную разновидность компонент, которые называются
дискриминантами:

.. code:: ada compile_button project=Courses.Intro_To_Ada.More_About_Records.Var_Size_Record_2

    package Var_Size_Record_2 is
        type Items_Array is array (Positive range <>)
          of Integer;

        type Growable_Stack (Max_Len : Natural) is
        record
        --                   ^ Discriminant. Cannot be
        --                     modified once initialized.
           Items : Items_Array (1 .. Max_Len);
           Len   : Natural := 0;
        end record;
        --  Growable_Stack is an indefinite type
        --  (like an array)
    end Var_Size_Record_2;

Дискриминанты, грубо говоря, являются константами: вы не можете изменять их
значение после инициализации объекта. Это интуитивно понятно, поскольку они
определяют размер объекта.

Кроме того, они делают тип неопределенным. В независимости от того,
используется ли дискриминант для указания размера объекта или нет, тип с
дискриминантом считается неопределенным, пока дискриминант не имеет выражение
для инициализации:

.. code:: ada compile_button project=Courses.Intro_To_Ada.More_About_Records.Test_Discriminants
    :class: ada-expect-compile-error

    package Test_Discriminants is
       type Point (X, Y : Natural) is record
          null;
       end record;

       P : Point;
       --  ERROR: Point is indefinite, so you
       --  need to specify the discriminants
       --  or give a default value

       P2 : Point (1, 2);
       P3 : Point := (1, 2);
       --  Those two declarations are equivalent.

    end Test_Discriminants;

Это также означает, что в приведенном выше примере вы не можете
объявить массив значений Point, потому что размер Point неизвестен.

Как упоминалось выше, мы могли бы предоставить
значение по умолчанию для дискриминантов, чтобы легально
объявлять переменные типа :ada:`Point` без указания значения дискриминантов.
В приведенном выше примере это будет выглядеть так:

.. code:: ada compile_button project=Courses.Intro_To_Ada.More_About_Records.Test_Discriminants

    package Test_Discriminants is
       type Point (X, Y : Natural := 0) is record
          null;
       end record;

       P : Point;
       --  We can now simply declare a "Point"
       --  without further ado. In this case,
       --  we're using the default values (0)
       --  for X and Y.

       P2 : Point (1, 2);
       P3 : Point := (1, 2);
       --  We can still specify discriminants.

    end Test_Discriminants;

Также обратите внимание, что, хотя тип :ada:`Point` теперь имеет дискриминанты по
умолчанию, это не мешает нам указывать дискриминанты, как мы это
делаем в объявлениях :ada:`P2` и :ada:`P3`.

Во многих других отношениях дискриминанты ведут себя как обычные поля:
вы должны указать их значения в агрегатах, как показано выше, и вы
можете извлекать их значения с помощью точечной нотации.

.. code:: ada run_button project=Courses.Intro_To_Ada.More_About_Records.Var_Size_Record_2

    with Var_Size_Record_2; use Var_Size_Record_2;
    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       procedure Print_Stack (G : Growable_Stack) is
       begin
          Put ("<Stack, items: [");
          for I in G.Items'Range loop
             exit when I > G.Len;
             Put (" " & Integer'Image (G.Items (I)));
          end loop;
          Put_Line ("]>");
       end Print_Stack;

       S : Growable_Stack :=
         (Max_Len => 128,
          Items   => (1, 2, 3, 4, others => <>),
          Len     => 4);
    begin
       Print_Stack (S);
    end Main;

.. note::
    В примере выше, мы использовали дискриминант чтобы указать размер
    массива, но возможны и другие применения, например,
    определение дискриминанта вложенной записи.

Записи c вариантами
-------------------

Ранее мы привели примеры использования дискриминантов для объявление
записей разлного размера, содержащих компоненты, размер которых зависит от
дискриминанта.

Но с помощью дискриминантов также можно построить конструкцию
часто именуемую «запись с вариантами»: это записи, которые могут содержать разные
наборы полей.

.. code:: ada compile_button project=Courses.Intro_To_Ada.More_About_Records.Variant_Record

    package Variant_Record is
       --  Forward declaration of Expr
       type Expr;

       --  Access to a Expr
       type Expr_Access is access Expr;

       type Expr_Kind_Type is (Bin_Op_Plus,
                               Bin_Op_Minus,
                               Num);
       --  A regular enumeration type

       type Expr (Kind : Expr_Kind_Type) is record
          --      ^ The discriminant is an
          --        enumeration value
          case Kind is
             when Bin_Op_Plus | Bin_Op_Minus =>
                Left, Right : Expr_Access;
             when Num =>
                Val : Integer;
          end case;
          --  Variant part. Only one, at the end of
          --  the record definition, but can be
          --  nested
       end record;
    end Variant_Record;

Поля, которые находятся в варианте :ada:`when`, будут доступны только тогда, когда
значение дискриминанта совпадает с указанным. В приведенном выше примере
вы сможете обращаться к полям :ada:`Left` и :ada:`Right`, только если
:ada:`Kind` равен :ada:`Bin_Op_Plus` или :ada:`Bin_Op_Minus`.

Если вы попытаетесь получить доступ к полю, когда значение дискриминанта не
совпадает, будет возбуждено исключение :ada:`Constraint_Error`.

.. code:: ada run_button project=Courses.Intro_To_Ada.More_About_Records.Variant_Record
    :class: ada-run-expect-failure

    with Variant_Record; use Variant_Record;

    procedure Main is
       E : Expr := (Num, 12);
    begin
       E.Left := new Expr'(Num, 15);
       --  Will compile but fail at runtime
    end Main;

А вот как можно написать вычислитель выражений:

.. code:: ada run_button project=Courses.Intro_To_Ada.More_About_Records.Variant_Record

    with Variant_Record; use Variant_Record;
    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       function Eval_Expr (E : Expr) return Integer is
         (case E.Kind is
          when Bin_Op_Plus  => Eval_Expr (E.Left.all)
                               + Eval_Expr (E.Right.all),
          when Bin_Op_Minus => Eval_Expr (E.Left.all)
                               - Eval_Expr (E.Right.all),
          when Num => E.Val);

       E : Expr := (Bin_Op_Plus,
                    new Expr'(Bin_Op_Minus,
                              new Expr'(Num, 12),
                              new Expr'(Num, 15)),
                    new Expr'(Num, 3));
    begin
       Put_Line (Integer'Image (Eval_Expr (E)));
    end Main;

.. admonition:: На других языках

    Записи вариантов Аде очень похожи на Sum типы в функциональных языках,
    таких как OCaml или Haskell. Основное отличие состоит в том, что
    дискриминант является отдельным полем в Аде, тогда как «тег» Sum типа
    является встроенным и доступен только при сопаставлении шаблонов.

    Есть и другие различия (записи с вариантами в Аде могут иметь несколько
    дискриминантов). Тем не менее, они допускают тот же подход к моделированию,
    что и типы Sum функциональных языков.

    По сравнению с объединениями C/C++ записи с вариантами Аде более мощны,
    а также благодаря проверкам во время выполнения, более безопасны.

