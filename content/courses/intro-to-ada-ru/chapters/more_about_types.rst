Подробнее о типах
=================

.. _Aggregates:

.. include:: ../../global.txt

Агрегаты: краткая информация
----------------------------

До сих пор мы говорили об агрегатах довольно мало и лишь видели ряд
примеров. Теперь мы вернемся к этой функции более подробно.

Агрегат Ada фактически является литеральным значением составного типа.
Это очень мощная нотация, которая помогает избежать написания
процедурного кода для инициализации структур данных во многих случаях.

Основным правилом при записи агрегатов является то, что *каждый
компонент* массива или записи должен быть указан, даже компоненты,
которые имеют значение по умолчанию.

Это означает, что следующий код неверен:

.. code-block:: ada
    :class: ada-expect-compile-error

    package Incorrect is
       type Point is record
          X, Y : Integer := 0;
       end record;

       Origin : Point := (X => 0);
    end Incorrect;

Существует несколько ярлыков, которые можно использовать, чтобы
сделать представление более удобным:

-  Чтобы задать значение по умолчанию для компонента, можно использовать
   нотацию « :ada:`<>` ».

-  Символ « :ada:`|` » можно использовать для присвоения нескольким компонентам
   одинакового значения.

-  Вы можете использовать :ada:`others` вариант для ссылки на все компоненты,
   которые еще не были указаны, при условии, что все эти поля имеют
   одинаковый тип.

-  Можно использовать нотацию диапазона « :ada:`..` » указывает непрерывную
   последовательность индексов в массиве.

Однако следует отметить, что, как только используется именованная
связь, все последующие компоненты также должны быть указаны с
ассоциациями имен.

.. code-block:: ada

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

Перегрузка и квалифицированные выражения
----------------------------------------

У Ada есть общая концепция перегрузки имен, которую мы видели ранее в
разделе о :ref:`типах перечисления <EnumTypes>`.

Давайте возьмем простой пример: в Ada возможно иметь функции с
одинаковым именем, но разными типами для их параметров.

.. code-block:: ada
    :class: ada-syntax-only

    package Pkg is
       function F (A : Integer) return Integer;
       function F (A : Character) return Integer;
    end Pkg;

Это распространенное понятие в языках программирования, называемое
перегрузкой, или `перегрузкой имен
<https://ru.m.wikipedia.org/wiki/Перегрузка_процедур_и_функций>`_.

Одним из новых аспектов средства перегрузки Ada является возможность
разрешить перегрузку на основе возвращаемого типа функции.

.. code-block:: ada
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
    Заметим, что разрешение перегрузки на основе типа разрешено как для
    функций, так и для литералов перечисления в Ada ‑ вот почему у вас
    может быть несколько литералов перечисления с одинаковым именем.
    Семантически литерал перечисления рассматривается как функция, не
    имеющая параметров.

Однако иногда двусмысленность делает невозможным определить, к какому
объявлению перегруженного имени относится данное вхождение имени.
Именно здесь становится полезным квалифицированное выражение.

.. code-block:: ada

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

Синтаксически целью квалифицированного выражения может быть любое
выражение в круглых скобках или агрегат:

.. code-block:: ada

    package Qual_Expr is
       type Point is record
          A, B : Integer;
       end record;

       P : Point := Point'(12, 15);

       A : Integer := Integer'(12);
    end Qual_Expr;

Это иллюстрирует, что квалифицированные выражения являются удобным (а
иногда и необходимым) способом для программиста сделать тип выражения
явным, конечно, для компилятора, но также и для других программистов.

.. attention::
    Хотя они выглядят и ощущаются похожими, преобразования типов и
    квалифицированные выражения *не* являются одинаковыми.

    Квалифицированное выражение указывает точный тип, в который будет
    преобразовано целевое выражение, в то время как преобразование типа
    попытается преобразовать целевое значение и выдаст ошибку во время
    выполнения, если целевое значение не может быть преобразовано таким
    образом.

    Обратите внимание, что вы можете использовать квалифицированное
    выражение для преобразования из одного подтипа в другой, с
    исключением, возникающим при нарушении ограничения.

    .. code-block:: ada

        X : Integer := Natural'(1);

Типы Access (указатели)
-----------------------

Указатели ‑ потенциально опасная конструкция, которая вступает в
противоречие с основополагающей философией Ады.

Есть два способа, которыми Ada помогает оградить программистов от
опасности указателей:

1. Один из подходов, который мы уже видели, заключается в обеспечении
   альтернативных функций, чтобы программисту не нужно было использовать
   указатели. Режимы параметров, массивы и типы различных размеров ‑ это
   все конструкции, которые могут заменить типичное использование
   указателей в C.

2. Во-вторых, Ada сделала указатели максимально безопасными и
   ограниченными, но допускает «спасательные люки», когда программист
   явно запрашивает их и, предположительно, будет осуществлять такие
   функции с соответствующей осторожностью.

Вот как в Ada объявляется простой тип указателя или тип доступа:

.. code-block:: ada

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

На этом рисунке показано, как:

-  Объявить тип доступа, значения которого указывают на («обозначить»)
   объекты из определенного типа
-  Объявить переменную (значение доступа) из этого типа доступа
-  Присвойте ему значение :ada:`null`

В соответствии со строгой философией типизации Ada, если вы объявите
второй тип доступа, обозначенный как Date, два типа доступа будут
несовместимы друг с другом, и вам потребуется явное преобразование
типа для преобразования из одного в другой:

.. code-block:: ada
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

    В большинстве других языков типы указателей структурно, а не
    номинально типизируются, как в Ada, что означает, что два типа
    указателей будут одинаковыми при условии, что они имеют один и тот же
    целевой тип и правила доступности.

    Не так в Ada, к которой нужно какое-то время привыкнуть. Казалось бы,
    простая проблема заключается в том, что если вы хотите иметь
    канонический доступ к типу, где он должен быть объявлен? Обычно
    используемый шаблон заключается в том, что если требуется тип доступа
    к определенному типу, которым вы «владеете», вы объявите его вместе с
    типом:

    .. code-block:: ada

        package Access_Types is
           type Point is record
              X, Y : Natural;
           end record;

           type Point_Access is access Point;
        end Access_Types;

Распределение (по типу) ‑ Allocation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

После того, как мы объявили тип доступа, нам нужен способ присвоить
переменным этих типов содержательное значение! Вы можете назначить
значение типа доступа с помощью нового ключевого слова :ada:`new` в Ada.

.. code-block:: ada

    with Dates; use Dates;

    package Access_Types is
        type Date_Acc is access Date;

        D : Date_Acc := new Date;
        --              ^ Allocate a new Date record
    end Access_Types;

Если тип, который требуется назначить, требует ограничений, их можно
поместить в указатель подтипа, как и в объявлении переменной:

.. code-block:: ada

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

Однако в некоторых случаях выделение только путем указания типа не
является идеальным, поэтому Ada также позволяет инициализировать
вместе с выделением. Это делается с помощью синтаксиса
квалифицированного выражения:

.. code-block:: ada

    with Dates; use Dates;

    package Access_Types is
       type Date_Acc is access Date;
       type String_Acc is access String;

       D   : Date_Acc   := new Date'(30, November, 2011);
       Msg : String_Acc := new String'("Hello");
    end Access_Types;

Разыменование
~~~~~~~~~~~~~

Важная последняя часть средства доступа типа access Ada заключается в
том, как получить значение доступа к объекту, на который указано, то
есть как разыменовать указатель. Для разыменования указателя
используется синтаксис :ada:`.all` в Ada, но часто в этом нет необходимости ‑ во
многих случаях значение доступа будет неявно разыменовано для вас:

.. code-block:: ada

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

Как вы, возможно, знаете, если вы использовали указатели в C или C++,
нам все еще не хватает функций, которые считаются основополагающими
для использования указателей, таких как:

-  Арифметика указателя (возможность увеличивать или уменьшать указатель,
   чтобы указывать на следующий или предыдущий объект)

-  Освобождение вручную ‑ то, что называется освобождением (:c:`free`) или
   удалением (:c:`delete`) в C. Это потенциально опасная операция. Чтобы оставаться
   в пределах безопасной Ada, вам никогда не нужно освобождать память
   вручную.

Эти функции существуют в Ada, но доступны только через определенные
стандартные библиотечные API.

.. attention::
    Руководство Ada гласит, что большую часть времени вы можете избегать
    распределения вручную, и вам следует это делать.

    Существует множество способов избежать распределения вручную,
    некоторые из которых были рассмотрены (например, режимы параметров).
    Язык также предоставляет библиотечные абстракции, чтобы избежать
    указателей:

    1. Одним из них является использование :ref:`контейнеров <Containers>`.
       Контейнеры помогают
       пользователям избегать указателей, поскольку память контейнеров
       управляется автоматически.

    2. Контейнер, который следует отметить в этом контексте, является
       `Indefinite holder <http://www.ada-auth.org/standards/12rat/html/Rat12-8-5.html>`_.
       Этот контейнер позволяет хранить значение неопределенного типа, например
       String.

    3. GNATCOLL имеет библиотеку для интеллектуальных указателей, называемую
       `Refcount <https://github.com/AdaCore/gnatcoll-core/blob/master/src/gnatcoll-refcount.ads>`_,
       память этих указателей автоматически управляется, так что, когда у
       выделенного объекта больше нет ссылок на него, память автоматически
       освобождается.

Взаимно рекурсивные типы
------------------------

Связанный список является общей идиомой в структурах данных; в Ada это
будет определено наиболее естественно через два типа, тип записи и тип
доступа, которые являются взаимно зависимыми. Для объявления взаимно
зависимых типов можно использовать неполное объявление типа:

.. code-block:: ada

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

Типы записей с динамически изменяемым размером
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Ранее мы видели несколько простых примеров типов записей. Давайте
рассмотрим некоторые из более продвинутых свойств этой фундаментальной
языковой функции.

Следует отметить, что размер объекта для типа записи не обязательно
должен быть известен во время компиляции. Это проиллюстрировано в
приведенном ниже примере:

.. code-block:: ada
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
время выполнения, но каждый экземпляр :ada:`Growable_Stack` будет точно такого же размера.
Но, возможно, это не то, что ты хочешь сделать. Мы видели, что массивы
в целом предлагают такую гибкость: для неограниченного типа массивов
разные объекты могут иметь разные размеры.

Можно также получить аналогичную функциональность для записей,
используя специальный вид поля, который называется дискриминантом:

.. code-block:: ada

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

Дискриминанты в их простых формах постоянны: вы не можете изменять их
после инициализации объекта. Это интуитивно понятно, поскольку они
определяют размер объекта.

Кроме того, они делают тип неопределенным: независимо от того,
используется ли дискриминант для указания размера объекта, тип с
дискриминантом будет неопределенным, если дискриминант не объявлен с
инициализацией:

.. code-block:: ada
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

Как упоминалось в приведенном выше примере, мы могли бы предоставить
значение по умолчанию для дискриминантов, чтобы мы могли легально
объявлять значения :ada:`Point` без указания дискриминантов. В приведенном выше
примере это будет выглядеть так:

.. code-block:: ada

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
умолчанию, мы все равно можем указывать дискриминанты, как мы это
делаем в объявлениях :ada:`P2` и :ada:`P3`.

Во многих других отношениях дискриминанты ведут себя как обычные поля:
вы должны указать их значения в агрегатах, как показано выше, и вы
можете получить доступ к их значениям через точечную нотацию.

.. code-block:: ada

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
    указание дискриминантов для вложенной записи.

Записи c вариантами
~~~~~~~~~~~~~~~~~~~

Примеры дискриминантов до сих пор проиллюстрировали объявление записей
различного размера с помощью компонентов, размер которых зависит от
дискриминанта.

Однако дискриминанты также могут использоваться для получения
функциональных возможностей того, что иногда называют «вариантными
записями»: записей, которые могут содержать различные наборы полей.

.. code-block:: ada

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

Поля, которые находятся в ветви :ada:`when`, будут доступны только тогда, когда
значение дискриминанта покрывается ветвью. В приведенном выше примере
вы сможете получить доступ к полям :ada:`Left` и :ada:`Right`, только если
:ada:`Kind` равен :ada:`Bin_Op_Plus` или :ada:`Bin_Op_Minus`.

Если вы попытаетесь получить доступ к полю, недопустимому для вашей
записи, будет вызвана ошибка :ada:`Constraint_Error`.

.. code-block:: ada
    :class: ada-run-expect-failure

    with Variant_Record; use Variant_Record;

    procedure Main is
       E : Expr := (Num, 12);
    begin
       E.Left := new Expr'(Num, 15);
       --  Will compile but fail at runtime
    end Main;

Вот как можно написать вычислитель для выражений:

.. code-block:: ada

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

    Записи вариантов Ada очень похожи на типы Sum в функциональных языках,
    таких как OCaml или Haskell. Основное отличие состоит в том, что
    дискриминант является отдельным полем в Ada, тогда как «тег» типа Sum
    является встроенным и доступен только при совпадении шаблонов.

    Есть и другие различия (в записи варианта в Ada можно иметь несколько
    дискриминантов). Тем не менее, они допускают тот же вид моделирования
    типа, что и типы Sum в функциональных языках.

    По сравнению с объединениями C/C + + записи вариантов Ada более мощны
    в том, что они позволяют, а также проверяются во время выполнения, что
    делает их более безопасными.

Типы с фиксированной точкой
---------------------------

Десятичные типы с фиксированной точкой
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Мы уже видели, как определять типы с плавающей запятой. Однако в
некоторых приложениях плавающая точка не подходит, поскольку,
например, ошибка округления из двоичной арифметики может быть
неприемлемой или, возможно, оборудование не поддерживает инструкции с
плавающей точкой. Ada предоставляет категорию типов, десятичные типы с
фиксированной запятой, которые позволяют программисту указать
требуемую десятичную точность (количество цифр), а также коэффициент
масштабирования (степень десяти) и, необязательно, диапазон.
Фактически, значения будут представлены как целые числа, неявно
масштабированные с указанной степенью 10. Это полезно, например, для
финансовых приложений.

Синтаксис простого десятичного типа с фиксированной запятой:

.. code-block:: ada

    type <type-name> is delta <delta-value> digits <digits-value>;

В этом случае :ada:`delta` и :ada:`digits` будут использоваться компилятором
для получения диапазона.

Несколько атрибутов полезны для работы с десятичными типами:

+------------------------+----------------------------------------------+
| Имя атрибута           | Значение                                     |
+========================+==============================================+
| First                  | Первое значение типа                         |
+------------------------+----------------------------------------------+
| Last                   | Последнее значение типа                      |
+------------------------+----------------------------------------------+
| Delta                  | Значение дельты типа                         |
+------------------------+----------------------------------------------+

В приведенном ниже примере мы объявляем два типа данных: :ada:`T3_D3` и :ada:`T6_D3`. Для
обоих типов значение дельты одинаково: 0.001.

.. code-block:: ada

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
типов с фиксированной запятой меньше, чем значение дельты, требуемое
для контекста, фактический результат будет равен нулю. Например:

.. code-block:: ada

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
того, что это значение не может быть представленно типом :ada:`T3_D3` поскольку
его дельта равна 0.001, реальное значение которое получит переменная :ada:`A`
будет равно нулю. Однако, если тип имеет большую точность, то
арифметические операции будут выполнятся достаточно точно, и значение
C будет равно 0.000500.

Типы фиксированных точек
~~~~~~~~~~~~~~~~~~~~~~~~

Обычные типы с фиксированной точкой похожи на десятичные типы с
фиксированной точкой в том, что значения, по сути, являются
масштабированными целыми числами. Разница между ними заключается в
коэффициенте масштабирования: для десятичного типа с фиксированной
точкой масштабирование, явно заданное дельтой (:ada:`delta`) типа,
всегда является степенью десяти. 

Напротив, для обычного типа с фиксированной точкой масштабирование
определяется значением :ada:`small` для типа, которое получается из указанной
:ada:`delta` и по умолчанию является степенью двойки. Поэтому обычные типы
с фиксированной точкой иногда называют двоичными типами с
фиксированной точкой.

.. note::
    Обычные типы с фиксированной точкой можно рассматривать как более
    близкие к реальному представлению на машине, поскольку аппаратная
    поддержка десятичной арифметики с фиксированной точкой не получила
    широкого распространения (изменение масштаба в десять раз), в то время
    как обычные типы с фиксированной точкой используют доступных
    инструкций целочисленного сдвига.

Синтаксис обычного типа с фиксированной точкой:

.. code-block:: ada

    type <type-name> is
      delta <delta-value>
      range <lower-bound> .. <upper-bound>;

По умолчанию компилятор выберет коэффициент масштабирования, или :ada:`small`, то
есть степень 2, не превышающую <delta-value>.

Например, можно определить нормированный диапазон между -1.0 и 1.0
следующим образом:

.. code-block:: ada

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
точкой для нормализованного диапазона. При запуске приложения мы
замечаем, что верхняя граница близка к единице, но не точная. Это
типичный эффект типов данных с фиксированной точкой - более подробную
информацию можно найти в этом обсуждении
`Q формата <https://en.wikipedia.org/wiki/Q_(number_format)>`_.
Мы также можем переписать этот код с точным определением типа:

.. code-block:: ada

    procedure Normalized_Adapted_Fixed_Point_Type is
       type TQ31 is
         delta 2.0 ** (-31)
         range -1.0 .. 1.0 - 2.0 ** (-31);
    begin
       null;
    end Normalized_Adapted_Fixed_Point_Type;

Мы также можем использовать любой другой диапазон. Например:

.. code-block:: ada

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

Для типов с фиксированной точкой доступны все стандартные операции.
Например:

.. code-block:: ada

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

Как и ожидалось, :ada:`R` содержит 0,75 после добавления :ada:`A` и :ada:`B`.

На самом деле язык является более общим, чем подразумевают эти
примеры, поскольку на практике обычно необходимо умножать или делить
значения из различных типов с фиксированной точкой и получать
результат, который может быть третьего типа с фиксированной точкой.
Подробная информация выходит за рамки данного вводного курса.

Следует также отметить, что, хотя подробности также выходят за рамки
данного курса, можно явно указать значение :ada:`small` для обычного типа с
фиксированной точкой. Это позволяет осуществлять небанальное
масштабирование, например:

.. code-block:: ada

    type Angle is
      delta 1.0/3600.0
      range 0.0 .. 360.0 - 1.0 / 3600.0;
    for Angle'Small use Angle'Delta;

Типы символов
-------------

Как отмечалось ранее, каждый тип перечисления отличается и несовместим
с каждым другим типом перечисления. Однако ранее мы не упоминали, что
литералы символов разрешены в качестве литералов перечисления. Это
означает, что в дополнение к строго типизированным типам символов
языка разрешены также определяемые пользователем типы символов:

.. code-block:: ada
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


