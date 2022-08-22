Подробнее о типах
=================

.. _Aggregates:

.. include:: ../../global.txt

Агрегаты: краткая информация
----------------------------

До сих пор мы говорили об агрегатах довольно мало и видели лишь ряд
примеров. Теперь мы рассмотрим эту конструкцию более подробно.

Агрегат в Аде фактически является литералом составного типа.
Это очень мощная форма записи во многих случаях позволяет избежать
написания процедурного кода для инициализации структур данных.

Основным правилом при записи агрегатов является то, что *каждый
компонент* массива или записи должен быть указан, даже компоненты,
которые имеют значение по умолчанию.

Это означает, что следующий код неверен:

.. code:: ada compile_button project=Courses.Intro_To_Ada.More_About_Types.Incorrect_Aggregate
    :class: ada-expect-compile-error

    package Incorrect is
       type Point is record
          X, Y : Integer := 0;
       end record;

       Origin : Point := (X => 0);
    end Incorrect;

Существует несколько сокращений, которые можно использовать, чтобы
сделать представление более удобным:

-  Чтобы задать значение по умолчанию для компоненты, можно использовать
   нотацию :ada:`<>`.

-  Символ :ada:`|` можно использовать для присвоения нескольким компонентам
   одинакового значения.

-  Вы можете использовать :ada:`others` вариант для ссылки на все компоненты,
   которые еще не были указаны, при условии, что все эти поля имеют
   одинаковый тип.

-  Можно использовать нотацию диапазона :ada:`..` чтобы указать непрерывную
   последовательность индексов в массиве.

Однако следует отметить, что, как только вы использовали именованное
сопоставление, все последующие компоненты также должны быть указаны с
помощью именованного сопоставления.

.. code:: ada compile_button project=Courses.Intro_To_Ada.More_About_Types.Points

    package Points is
       type Point is record
          X, Y : Integer := 0;
       end record;

       type Point_Array is
         array (Positive range <>) of Point;

       --  use the default values
       Origin   : Point := (X | Y => <>);

       --  likewise, use the defaults
       Origin_2 : Point := (others => <>);

       Points_1 : Point_Array := ((1, 2), (3, 4));
       Points_2 : Point_Array := (1       => (1, 2),
                                  2       => (3, 4),
                                  3 .. 20 => <>);
    end Points;

Cовмещение и квалифицированные выражения
----------------------------------------

В Ада есть общая концепция совмещения имен, которую мы видели ранее в
разделе о :ref:`перечислимых типах <EnumTypes>`.

Давайте возьмем простой пример: в Аде возможно иметь функции с
одинаковым именем, но разными типами для их параметров.

.. code:: ada no_button project=Courses.Intro_To_Ada.More_About_Types.Overloading
    :class: ada-syntax-only

    package Pkg is
       function F (A : Integer) return Integer;
       function F (A : Character) return Integer;
    end Pkg;

Это распространенное понятие в языках программирования, называемое
совмещением, или `перегрузкой имен
<https://ru.m.wikipedia.org/wiki/Перегрузка_процедур_и_функций>`_.

Одной из особенностей совмещения имен в Аде является возможность
разрешить неоднозначности на основе типа возвращаемого функцией.

.. code:: ada no_button project=Courses.Intro_To_Ada.More_About_Types.Overloading
    :class: ada-syntax-only

    package Pkg is
       type SSID is new Integer;

       function Convert (Self : SSID) return Integer;
       function Convert (Self : SSID) return String;
    end Pkg;

    with Ada.Text_IO; use Ada.Text_IO;
    with Pkg;         use Pkg;

    procedure Main is
       S : String := Convert (123_145_299);
       --            ^ Valid, will choose the
       --              proper Convert
    begin
       Put_Line (S);
    end Main;

.. attention::
    Заметим, что разрешение совмещения на основе типа в Аде работает как для
    функций, так и для литералов перечисления - вот почему у вас
    может быть несколько литералов перечисления с одинаковым именем.
    Семантически литерал перечисления рассматривается как функция, не
    имеющая параметров.

Однако иногда возникает двусмысленность из-за которой невозможно определить,
к какому объявлению совмещенного имени относится данное употребление имени.
Именно здесь становится полезным квалифицированное выражение.

.. code:: ada no_button project=Courses.Intro_To_Ada.More_About_Types.Overloading_Error
    :class: ada-syntax-only, ada-expect-compile-error

    package Pkg is
       type SSID is new Integer;

       function Convert (Self : SSID) return Integer;
       function Convert (Self : SSID) return String;
       function Convert (Self : Integer) return String;
    end Pkg;

    with Ada.Text_IO; use Ada.Text_IO;
    with Pkg;         use Pkg;

    procedure Main is
       S : String := Convert (123_145_299);
       --            ^ Invalid, which convert
       --              should we call?

       S2 : String := Convert (SSID'(123_145_299));
       --                     ^ We specify that the
       --                       type of the expression
       --                       is SSID.

       --  We could also have declared a temporary

       I : SSID := 123_145_299;

       S3 : String := Convert (I);
    begin
       Put_Line (S);
    end Main;

Синтаксически квалифицированное выражение используется либо с
выражением в круглых скобках, либо с агрегатом:

.. code:: ada compile_button project=Courses.Intro_To_Ada.More_About_Types.Qual_Expr

    package Qual_Expr is
       type Point is record
          A, B : Integer;
       end record;

       P : Point := Point'(12, 15);

       A : Integer := Integer'(12);
    end Qual_Expr;

Это иллюстрирует, что квалифицированные выражения являются удобным (а
иногда и необходимым) способом явно обозначить тип выражения
и помочь, как компилятору, так и для другим программистам разобраться
в коде.

.. attention::
    Хотя преобразования типов и квалифицированные выражения
    выглядят и ощущаются похожими это *не* одно и тоже.

    Квалифицированное выражение указывает точный тип, в котором следует
    трактовать выражение, в то время как преобразование типа
    попытается преобразовать значение и выдаст ошибку во время
    выполнения, если исходное значение не может быть преобразовано.

    Обратите внимание, что вы можете использовать квалифицированное
    выражение для преобразования из одного подтипа в другой, с
    исключением, возникающим при нарушении ограничения.

    .. code-block:: ada

        X : Integer := Natural'(1);

Ссылочные типы (указатели)
--------------------------

Указатели - потенциально опасная конструкция, которая вступает в
противоречие с основополагающей философией Ады.

Есть два способа, которыми Ада помогает оградить программистов от
опасности указателей:

1. Один из подходов, который мы уже видели, заключается в обеспечении
   альтернативных возможностей, чтобы программисту не нужно было использовать
   указатели. Виды параметров, массивы и типы произвольных размеров - это
   все конструкции, которые могут заменить типичное использование
   указателей в C.

2. Во-вторых, Ада сделала указатели максимально безопасными и
   ограниченными, хотя допускает «аварийные люки», которые программист
   явно затребовать и, предположительно, будет использовать их
   с соответствующей осторожностью.

Вот как в Аде объявляется простой тип указателя, ссылочный тип:

.. code:: ada compile_button project=Courses.Intro_To_Ada.More_About_Types.Access_Types

    package Dates is
       type Months is
         (January, February, March, April,
          May, June, July, August, September,
          October, November, December);

       type Date is record
          Day   : Integer range 1 .. 31;
          Month : Months;
          Year  : Integer;
       end record;
    end Dates;

    with Dates; use Dates;

    package Access_Types is
        --  Declare an access type
        type Date_Acc is access Date;
        --                      ^ "Designated type"
        --                      ^ Date_Acc values point
        --                        to Date objects

        D : Date_Acc := null;
        --              ^ Literal for
        --                "access to nothing"
        --  ^ Access to date
    end Access_Types;

На этом примере показано, как:

-  Объявить ссылочный тип, значения которого *указывают*
   на объекты определенного типа
-  Объявить переменную (для ссылочных значений) этого ссылочного типа
-  Присвоить ему значение :ada:`null`

В соответствии с философией строгой типизации Ада, если объявить
второй ссылочный тип, указывающий на Date, эти два ссылочных типа будут
несовместимы друг с другом:

.. code:: ada compile_button project=Courses.Intro_To_Ada.More_About_Types.Access_Types
    :class: ada-expect-compile-error

    with Dates; use Dates;

    package Access_Types is
        --  Declare an access type
        type Date_Acc   is access Date;
        type Date_Acc_2 is access Date;

        D  : Date_Acc   := null;
        D2 : Date_Acc_2 := D;
        --                 ^ Invalid! Different types

        D3 : Date_Acc_2 := Date_Acc_2 (D);
        --                 ^ Valid with type conversion
    end Access_Types;

.. admonition:: На других языках

    Большинство других языков используют структурно типизацию для
    указателей, что означает, что два типа
    указателей считаются одинаковыми, если они имеют один и тот же
    целевой тип.

    В Аде это не так, и к этому, возможно, придется какое-то время привыкать.
    Казалось бы, простой вопрос, если вы хотите для вашего типа иметь
    канонический ссылочный тип, где его объявить? Обычно
    используют следующий подход, если понадобится ссылочный тип для
    некоторого вашего типа, то вы объявите его вместе с типом:

    .. code-block:: ada

        package Access_Types is
           type Point is record
              X, Y : Natural;
           end record;

           type Point_Access is access Point;
        end Access_Types;

Выделение (allocation) памяти
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

После того, как мы объявили ссылочный тип, нам нужен способ присвоить
переменным этого типа осмысленные значения! Вы можете получить
значение ссылочного типа с помощью ключевого слова :ada:`new`.

.. code:: ada compile_button project=Courses.Intro_To_Ada.More_About_Types.Access_Types

    with Dates; use Dates;

    package Access_Types is
        type Date_Acc is access Date;

        D : Date_Acc := new Date;
        --              ^ Allocate a new Date record
    end Access_Types;

Если тип, память для значение которого требуется выделить, требует ограничений, их можно
указать после подтипа, как в объявлении переменной:

.. code:: ada compile_button project=Courses.Intro_To_Ada.More_About_Types.Access_Types

    with Dates; use Dates;

    package Access_Types is
       type String_Acc is access String;
       --                        ^
       --  Access to unconstrained array type
       Msg : String_Acc;
       --    ^ Default value is null

       Buffer : String_Acc :=
         new String (1 .. 10);
       --            ^ Constraint required
    end Access_Types;

Однако, в некоторых случаях выделение памяти путем указания типа не
является идеальным, поэтому Ада позволяет инициализировать объект
одновременно с выделением памяти. Чтобы сделать это необходимо использовать
квалифицированное выражение:

.. code:: ada compile_button project=Courses.Intro_To_Ada.More_About_Types.Access_Types

    with Dates; use Dates;

    package Access_Types is
       type Date_Acc is access Date;
       type String_Acc is access String;

       D   : Date_Acc   := new Date'(30, November, 2011);
       Msg : String_Acc := new String'("Hello");
    end Access_Types;

Извлечение по ссылке
~~~~~~~~~~~~~~~~~~~~

Последняя часть мозайки ссылочных типов языка Ада покажет нам, как получить
значение объекту по ссылке, то есть как «разыменовать» указатель.
Для этого в Аде используется синтаксис :ada:`.all`, но часто в нем
вообще нет необходимости - во многих случаях использования ссылочного значения
эта операция выполняется неявно:

.. code:: ada compile_button project=Courses.Intro_To_Ada.More_About_Types.Access_Types

    with Dates; use Dates;

    package Access_Types is
       type Date_Acc is access Date;

       D     : Date_Acc := new Date'(30, November, 2011);

       Today : Date := D.all;
       --              ^ Access value dereference
       J     : Integer := D.Day;
       --                 ^ Implicit dereference for
       --                   record and array components
       --                   Equivalent to D.all.day
    end Access_Types;

Другие особенности
~~~~~~~~~~~~~~~~~~

Как вы, возможно, заметили, если пользовались указатели в C или C++,
мы не показали некоторых функций, которые считаются основополагающими
при использования указателей, таких как:

-  Арифметика над указателями (возможность увеличивать или уменьшать указатель,
   чтобы переместить его на следующий или предыдущий объект)

-  Освобождение памяти вручную - то, что в C делается с помощью :c:`free` или
   :c:`delete`. Это потенциально опасная операция. Чтобы оставаться
   в безопасном пространстве Ады, вам лучше не освобождать память
   вручную.

Эти функции существуют в Аде, но воспользоваться ими можно только с помощью
определенных интерфейсов стандартной библиотеки (API).

.. attention::
    Общепринятый принцип языка гласит, что в большенстве случаев вы можете
    избегать ручного управления памятью, и вам лучше следовать ему.

    Существует множество способов избежать распределения памяти вручную,
    с некоторыми из них мы уже встречались (например, виды параметров).
    Язык также предоставляет библиотечные абстракции, чтобы избежать
    указателей:

    1. Одним из них является использование :ref:`контейнеров <Containers>`.
       Контейнеры помогают
       пользователям избегать указателей, поскольку сами управляют памятью.

    2. Контейнер, который следует отметить в этом контексте, является
       `Indefinite holder <http://www.ada-auth.org/standards/12rat/html/Rat12-8-5.html>`_.
       Этот контейнер позволяет хранить значение неопределенного типа, например
       String.

    3. GNATCOLL🤢 имеет библиотеку для интеллектуальных указателей, называемую
       `Refcount <https://github.com/AdaCore/gnatcoll-core/blob/master/src/gnatcoll-refcount.ads>`_,
       память этих указателей автоматически управляется, так что, когда у
       выделенного объекта больше нет ссылок на него, память автоматически
       освобождается.

Взаимно рекурсивные типы
~~~~~~~~~~~~~~~~~~~~~~~~

Связанный список является широко известной идиомой в программировании; в Аде
его наиболее естественная запись включает определение двух типов - тип записи
и ссфлочный тип, которые будут взаимно зависимыми. Для объявления взаимно
зависимых типов можно использовать неполное объявление типа:

.. code:: ada compile_button project=Courses.Intro_To_Ada.More_About_Types.Simple_List

    package Simple_List is
       type Node;
       --  This is an incomplete type declaration,
       --  which is completed in the same
       --  declarative region.

       type Node_Acc is access Node;

       type Node is record
          Content    : Natural;
          Prev, Next : Node_Acc;
       end record;
    end Simple_List;

Подробнее о записях
-------------------

Типы записей динамически изменяемого размера
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Ранее мы видели
:ref:`несколько простых примеров типов записей <Intro_Ada_Record_Type_Declaration>`.
Давайте рассмотрим некоторые из более продвинутых возможностей этой фундаментальной
конструкции языка Ада.

Следует отметить, что размер объекта для типа записи не обязательно
должен быть известен во время компиляции. Это проиллюстрировано в
приведенном ниже примере:

.. code:: ada no_button project=Courses.Intro_To_Ada.More_About_Types.Var_Size_Record
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
~~~~~~~~~~~~~~~~~~~~~~~

В приведенном выше примере размер поля Items определяется один раз во
время выполнения, но размер всех экземпляров :ada:`Growable_Stack` будет
совпадать.
И, возможно, это не то, что вы хотите получить. Мы видели, что массивы
в целом имеют такую гибкость: для неограниченного типа массива
разные объекты могут иметь разные размеры.

Получить аналогичную функциональность для записей можно
используя специальную разновидность компонент, которые называются
дискриминантами:

.. code:: ada compile_button project=Courses.Intro_To_Ada.More_About_Types.Var_Size_Record_2

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

.. code:: ada compile_button project=Courses.Intro_To_Ada.More_About_Types.Test_Discriminants
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

.. code:: ada compile_button project=Courses.Intro_To_Ada.More_About_Types.Test_Discriminants

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

.. code:: ada run_button project=Courses.Intro_To_Ada.More_About_Types.Var_Size_Record_2

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
~~~~~~~~~~~~~~~~~~~

Ранее мы привели примеры использования дискриминантов для объявление
записей разлного размера, содержащих компоненты, размер которых зависит от
дискриминанта.

Но с помощью дискриминантов также можно построить конструкцию
часто именуемую «запись с вариантами»: это записи, которые могут содержать разные
наборы полей.

.. code:: ada compile_button project=Courses.Intro_To_Ada.More_About_Types.Variant_Record

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

.. code:: ada run_button project=Courses.Intro_To_Ada.More_About_Types.Variant_Record
    :class: ada-run-expect-failure

    with Variant_Record; use Variant_Record;

    procedure Main is
       E : Expr := (Num, 12);
    begin
       E.Left := new Expr'(Num, 15);
       --  Will compile but fail at runtime
    end Main;

А вот как можно написать вычислитель выражений:

.. code:: ada run_button project=Courses.Intro_To_Ada.More_About_Types.Variant_Record

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

Типы с фиксированной запятой
----------------------------

Десятичные типы с фиксированной запятой
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Мы уже видели, как определять типы с плавающей запятой. Однако в
некоторых приложениях плавающая запятая не подходит,
например, ошибка округления при двоичной арифметики
неприемлема или, оборудование не поддерживает инструкции с
плавающей запятой. В языке Ада есть десятичные типы с
фиксированной запятой, которые позволяют программисту указать
требуемую десятичную точность (количество цифр), а также коэффициент
масштабирования (степень десяти) и, необязательно, диапазон.
Фактически, значения такого типа будут представлены как целые числа, неявно
масштабированные с указанной степенью 10. Это полезно, например, для
финансовых приложений.

Синтаксис простого десятичного типа с фиксированной запятой:

.. code-block:: ada

    type <type-name> is delta <delta-value> digits <digits-value>;

В этом случае :ada:`delta` и :ada:`digits` будут использоваться компилятором
для вычисления диапазона значений.

Несколько атрибутов полезны при работе с десятичными типами:

+------------------------+----------------------------------------------+
| Имя атрибута           | Значение                                     |
+========================+==============================================+
| First                  | Наименьшее значение типа                     |
+------------------------+----------------------------------------------+
| Last                   | Наибольшее значение типа                     |
+------------------------+----------------------------------------------+
| Delta                  | Значение минимального шага типа              |
+------------------------+----------------------------------------------+

В приведенном ниже примере мы объявляем два типа данных: :ada:`T3_D3` и :ada:`T6_D3`. Для
обоих типов значение дельты одинаково: 0.001.

.. code:: ada run_button project=Courses.Intro_To_Ada.More_About_Types.Decimal_Fixed_Point_Types

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Decimal_Fixed_Point_Types is
       type T3_D3 is delta 10.0 ** (-3) digits 3;
       type T6_D3 is delta 10.0 ** (-3) digits 6;
    begin
       Put_Line ("The delta    value of T3_D3 is "
                 & T3_D3'Image (T3_D3'Delta));
       Put_Line ("The minimum  value of T3_D3 is "
                 & T3_D3'Image (T3_D3'First));
       Put_Line ("The maximum  value of T3_D3 is "
                 & T3_D3'Image (T3_D3'Last));
       New_Line;

       Put_Line ("The delta    value of T6_D3 is "
                 & T6_D3'Image (T6_D3'Delta));
       Put_Line ("The minimum  value of T6_D3 is "
                 & T6_D3'Image (T6_D3'First));
       Put_Line ("The maximum  value of T6_D3 is "
                 & T6_D3'Image (T6_D3'Last));
    end Decimal_Fixed_Point_Types;

При запуске приложения мы видим, что значение дельты обоих типов
действительно одинаково: 0.001. Однако, поскольку :ada:`T3_D3` ограничен 3
цифрами, его диапазон составляет от -0,999 до 0,999. Для :ada:`T6_D3` мы
определили точность 6 цифр, поэтому диапазон от -999,999 до 999,999.

Аналогично определению типа с использованием синтаксиса диапазона
(:ada:`range`),
поскольку у нас есть неявный диапазон, скомпилированный код будет
проверять, что переменные содержат значения, не выходящие за пределы
диапазона. Кроме того, если результат умножения или деления десятичных
типов с фиксированной запятой меньше, чем значение дельты, известное из
контекста, фактический результат будет равен нулю. Например:

.. code:: ada run_button project=Courses.Intro_To_Ada.More_About_Types.Decimal_Fixed_Point_Smaller

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Decimal_Fixed_Point_Smaller is
       type T3_D3 is delta 10.0 ** (-3) digits 3;
       type T6_D6 is delta 10.0 ** (-6) digits 6;
       A : T3_D3 := T3_D3'Delta;
       B : T3_D3 := 0.5;
       C : T6_D6;
    begin
       Put_Line ("The value of A     is "
                 & T3_D3'Image (A));

       A := A * B;
       Put_Line ("The value of A * B is "
                 & T3_D3'Image (A));

       A := T3_D3'Delta;
       C := A * B;
       Put_Line ("The value of A * B is "
                 & T6_D6'Image (C));
    end Decimal_Fixed_Point_Smaller;

В этом примере, результат операции 0.001 * 0.5 будет 0.0005. Ввиду
того, что это значение не может быть представленно типом :ada:`T3_D3` ведь
его дельта равна 0.001, реальное значение которое получит переменная :ada:`A`
будет равно нулю. Однако, если тип имеет большую точность, то
точности арифметических операций будет достаточно, и значение
:ada:`C` будет равно 0.000500.

Обычные типы с фиксированной запятой
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Обычные типы с фиксированной запятой похожи на десятичные типы с
фиксированной запятой в том, что значения, по сути, являются
масштабированными целыми числами. Разница между ними заключается в
коэффициенте масштабирования: для десятичного типа с фиксированной
запятой масштабирование, явно заданное дельтой (:ada:`delta`),
всегда является степенью десяти.

Напротив, для обычного типа с фиксированной запятой масштабирование
определяется значением :ada:`small` для типа, которое получается из указанного
значения :ada:`delta` и, по умолчанию, является степенью двойки.
Поэтому обычные типы с фиксированной запятой иногда называют двоичными типами
с фиксированной запятой.

.. note::
    Обычные типы с фиксированной запятой можно рассматривать как более
    близкие к реальному представлению на машине, поскольку аппаратная
    поддержка десятичной арифметики с фиксированной запятой не получила
    широкого распространения (изменение масштаба в десять раз), в то время
    как обычные типы с фиксированной запятой доступных используют широко
    распространенные инструкций целочисленного сдвига.

Синтаксис обычного типа с фиксированной запятой:

.. code-block:: ada

    type <type-name> is
      delta <delta-value>
      range <lower-bound> .. <upper-bound>;

По умолчанию компилятор выберет коэффициент масштабирования, или :ada:`small`, то
есть степень 2, не превышающую <delta-value>.

Например, можно определить нормализованный диапазон между -1.0 и 1.0
следующим образом:

.. code:: ada run_button project=Courses.Intro_To_Ada.More_About_Types.Normalized_Fixed_Point_Type

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Normalized_Fixed_Point_Type is
       D : constant := 2.0 ** (-31);
       type TQ31 is delta D range -1.0 .. 1.0 - D;
    begin
       Put_Line ("TQ31 requires "
                 & Integer'Image (TQ31'Size)
                 & " bits");
       Put_Line ("The delta    value of TQ31 is "
                 & TQ31'Image (TQ31'Delta));
       Put_Line ("The minimum  value of TQ31 is "
                 & TQ31'Image (TQ31'First));
       Put_Line ("The maximum  value of TQ31 is "
                 & TQ31'Image (TQ31'Last));
    end Normalized_Fixed_Point_Type;

В этом примере мы определяем 32-разрядный тип данных с фиксированной
запятой для нормализованного диапазона. При запуске приложения мы
замечаем, что верхняя граница близка к единице, но не ровна. Это
типичный эффект типов данных с фиксированной запятой - более подробную
информацию можно найти в этом обсуждении
`Q формата <https://en.wikipedia.org/wiki/Q_(number_format)>`_.
Мы также можем переписать этот код определения типа:

.. code:: ada compile_button project=Courses.Intro_To_Ada.More_About_Types.Normalized_Adapted_Fixed_Point_Type

    procedure Normalized_Adapted_Fixed_Point_Type is
       type TQ31 is
         delta 2.0 ** (-31)
         range -1.0 .. 1.0 - 2.0 ** (-31);
    begin
       null;
    end Normalized_Adapted_Fixed_Point_Type;

Мы также можем использовать любой другой диапазон. Например:

.. code:: ada run_button project=Courses.Intro_To_Ada.More_About_Types.Custom_Fixed_Point_Range

    with Ada.Text_IO;  use Ada.Text_IO;
    with Ada.Numerics; use Ada.Numerics;

    procedure Custom_Fixed_Point_Range is
       type T_Inv_Trig is
         delta 2.0 ** (-15) * Pi
         range -Pi / 2.0 .. Pi / 2.0;
    begin
       Put_Line ("T_Inv_Trig requires "
                 & Integer'Image (T_Inv_Trig'Size)
                 & " bits");
       Put_Line ("The delta    value of T_Inv_Trig is "
                 & T_Inv_Trig'Image (T_Inv_Trig'Delta));
       Put_Line ("The minimum  value of T_Inv_Trig is "
                 & T_Inv_Trig'Image (T_Inv_Trig'First));
       Put_Line ("The maximum  value of T_Inv_Trig is "
                 & T_Inv_Trig'Image (T_Inv_Trig'Last));
    end Custom_Fixed_Point_Range;

В этом примере мы определяем 16-разрядный тип с именем :ada:`T_Inv_Trig`, который имеет
диапазон от −𝜋/2 до 𝜋/2.

Для типов с фиксированной запятой доступны все общепринятые операции.
Например:

.. code:: ada run_button project=Courses.Intro_To_Ada.More_About_Types.Fixed_Point_Op

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Fixed_Point_Op is
       type TQ31 is
         delta 2.0 ** (-31)
         range -1.0 .. 1.0 - 2.0 ** (-31);

       A, B, R : TQ31;
    begin
       A := 0.25;
       B := 0.50;
       R := A + B;
       Put_Line ("R is " & TQ31'Image (R));
    end Fixed_Point_Op;

Как и ожидалось, :ada:`R` содержит 0,75 после сложения :ada:`A` и :ada:`B`.

На самом деле язык является более гибким, чем показано в этих
примерах, поскольку на практике обычно необходимо умножать или делить
значения различных типов с фиксированной запятой и получать
результат, который может быть третьего типа.
Подробная информация выходит за рамки данного вводного курса.

Следует также отметить, что, хотя подробности также выходят за рамки
данного курса, можно явно указать значение :ada:`small` для обычного типа с
фиксированной точкой. Это позволяет осуществлять недвоичное
масштабирование, например:

.. code-block:: ada

    type Angle is
      delta 1.0/3600.0
      range 0.0 .. 360.0 - 1.0 / 3600.0;
    for Angle'Small use Angle'Delta;

Символьные типы
---------------

Как отмечалось ранее, каждый перечислимый тип отличается и несовместим
с любым другим перечислимым типом. Однако ранее мы не упоминали, что
символьные литералы разрешены в качестве литералов перечисления. Это
означает, что в дополнение к стандартным символьным типам пользователь
может определить свои символьные типы:

.. code:: ada compile_button project=Courses.Intro_To_Ada.More_About_Types.Character_Example
    :class: ada-expect-compile-error

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Character_Example is
       type My_Char is ('a', 'b', 'c');
       --  Our custom character type, an
       --  enumeration type with 3 valid values.

       C : Character;
       --  ^ Built-in character type
       --    (it's an enumeration type)

       M : My_Char;
    begin
       C := '?';
       --   ^ Character literal
       --     (enumeration literal)

       M := 'a';

       C := 65;
       --   ^ Invalid: 65 is not a
       --     Character value

       C := Character'Val (65);
       --  Assign the character at
       --  position 65 in the
       --  enumeration (which is 'A')

       M := C;
       --   ^ Invalid: C is of type Character,
       --     and M is a My_Char

       M := 'd';
       --   ^ Invalid: 'd' is not a valid
       --     literal for type My_Char
    end Character_Example;


