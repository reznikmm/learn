Стандартная библиотека: Строки
==============================

.. include:: ../../global.txt

В предыдущих главах мы видели примеры исходного кода с использованием
типа :ada:`String`, который является строковым типом фиксированной
длины - по сути, это массив символов. Во многих случаях этого типа данных
достаточно для работы с текстовой информацией. Однако бывают ситуации, когда
требуется более сложная обработка текста. Ada предлагает
альтернативные подходы для этих случаев:

-  *Ограниченные строки*: аналогично строкам фиксированной длины,
   ограниченные строки имеют максимальную длину, которая устанавливается
   при их создании. Однако ограниченные строки не являются массивами
   символов. В любой момент они могут содержать строку различной длины -
   при условии, что эта длина меньше или равна максимальной длине.

-  *Неограниченные строки*: подобно ограниченным строкам, неограниченные
   строки могут содержать строки различной длины. Однако, помимо этого, у
   них нет максимальной длины. В этом смысле они очень гибкие. 

В следующих разделах представлен обзор различных типов строк и общих
операций для типов строк.

Операции со строками
--------------------

Операции со стандартными строками (фиксированной длины) доступны в
пакете :ada:`Ada.Strings.Fixed`. Как упоминалось ранее, стандартные строки
представляют собой массивы элементов типа :ada:`Character`
с *фиксированной длиной*. Вот почему этот дочерний пакет называется
фиксированным (:ada:`Fixed`).

Одна из самых простых операций - это подсчет количества подстрок,
доступных в строке (:ada:`Count`), и поиск их соответствующих
индексов (:ada:`Index`). Давайте посмотрим на пример:

.. code-block:: ada

    with Ada.Strings.Fixed; use Ada.Strings.Fixed;
    with Ada.Text_IO;       use Ada.Text_IO;

    procedure Show_Find_Substring is

       S   : String := "Hello" & 3 * " World";
       P   : constant String := "World";
       Idx : Natural;
       Cnt : Natural;
    begin
       Cnt := Ada.Strings.Fixed.Count
         (Source  => S,
          Pattern => P);

       Put_Line ("String: " & S);
       Put_Line ("Count for '" & P & "': "
                 & Natural'Image (Cnt));

       Idx := 0;
       for I in 1 .. Cnt loop
          Idx := Index
            (Source  => S,
             Pattern => P,
             From    => Idx + 1);

          Put_Line ("Found instance of '"
                    & P & "' at position: "
                    & Natural'Image (Idx));
       end loop;

    end Show_Find_Substring;

Мы инициализируем строку :ada:`S` умножением. Запись
:ada:`"Hello" & 3 * " World"` создает строку ``Hello World World World``.
Затем мы вызываем функцию ``Count``, чтобы получить количество экземпляров
слова :ada:`World` в :ada:`S` .Затем мы вызываем функцию :ada:`Index` в
цикле, чтобы найти индекс каждого экземпляра слова :ada:`World` в :ada:`S`.

В этом примере искались экземпляры определенной подстроки. В следующем
примере мы извлекаем все слова в строке. Мы делаем это с помощью
:ada:`Find_Token` и определяем пробелы в качестве разделителей. Например:

.. code-block:: ada

    with Ada.Strings;       use Ada.Strings;
    with Ada.Strings.Fixed; use Ada.Strings.Fixed;
    with Ada.Strings.Maps;  use Ada.Strings.Maps;
    with Ada.Text_IO;       use Ada.Text_IO;

    procedure Show_Find_Words is

       S   : String := "Hello" & 3 * " World";
       F   : Positive;
       L   : Natural;
       I   : Natural := 1;

       Whitespace : constant Character_Set :=
         To_Set (' ');
    begin
       Put_Line ("String: " & S);
       Put_Line ("String length: "
                 & Integer'Image (S'Length));

       while I in S'Range loop
          Find_Token
            (Source  => S,
             Set     => Whitespace,
             From    => I,
             Test    => Outside,
             First   => F,
             Last    => L);

          exit when L = 0;

          Put_Line ("Found word instance at position "
                    & Natural'Image (F)
                    & ": '" & S (F .. L) & "'");
          --   & "-" & F'Img & "-" & L'Img

          I := L + 1;
       end loop;
    end Show_Find_Words;

Мы передаем набор символов, которые будут использоваться в качестве
разделителей для процедуры :ada:`Find_Token`. Этот набор является членом
типа :ada:`Character_Set` из пакета :ada:`Ada.Strings.Maps`. Мы вызываем
функцию :ada:`To_Set` (из того же пакета), чтобы инициализировать набор для
:ada:`Whitespace`, а затем вызываем :ada:`Find_Token`, чтобы перебирать
каждый допустимый индекс и находить начальный индекс каждого слова. Мы
передаем :ada:`Outside` в параметр :ada:`Test` процедуры :ada:`Find_Token`,
чтобы указать, что мы ищем индексы, которые находятся за пределами набора
:ada:`Whitespace`, то есть фактические слова. :ada:`First` и :ada:`Last`
параметры :ada:`Find_Token` являются выходными параметрами, которые указывают
допустимый диапазон подстроки. Мы используем эту информацию для
отображения строки (:ada:`S (F .. L)`).

Операции, которые мы рассмотрели до сих пор, читают строки, но не
изменяют их. Далее мы обсудим операции, которые изменяют содержимое
строк:

+-----------------------+-----------------------------------------+
| Operation             | Description                             |
+=======================+=========================================+
| Insert                | Insert substring in a string            |
+-----------------------+-----------------------------------------+
| Overwrite             | Overwrite a string with a substring     |
+-----------------------+-----------------------------------------+
| Delete                | Delete a substring                      |
+-----------------------+-----------------------------------------+
| Trim                  | Remove whitespaces from a string        |
+-----------------------+-----------------------------------------+

Все эти операции доступны как в виде функций, так и в виде процедур.
Функции создают новую строку, но процедуры выполняют операции на
месте. Процедура вызовет исключение, если ограничения строки не
выполнены. Например, если у нас есть строка :ada:`S`, содержащая 10 символов,
вставка в нее строки с двумя символами (например, :ada:`"!!"`) дает строку,
содержащую 12 символов. Поскольку он имеет фиксированную длину, мы не
можем увеличить его размер. Одно из возможных решений в этом случае -
указать, что при вставке подстроки следует применять усечение. Это
сохраняет длину :ada:`S`. Давайте посмотрим на пример, в котором используются
как функции, так и версии процедур :ada:`Insert`, :ada:`Overwrite` и
:ada:`Delete`:

.. code-block:: ada

    with Ada.Strings;       use Ada.Strings;
    with Ada.Strings.Fixed; use Ada.Strings.Fixed;
    with Ada.Text_IO;       use Ada.Text_IO;

    procedure Show_Adapted_Strings is

       S   : String := "Hello World";
       P   : constant String := "World";
       N   : constant String := "Beautiful";

       procedure Display_Adapted_String
         (Source   : String;
          Before   : Positive;
          New_Item : String;
          Pattern  : String)
       is
          S_Ins_In : String := Source;
          S_Ovr_In : String := Source;
          S_Del_In : String := Source;

          S_Ins : String :=
            Insert (Source,
                    Before,
                    New_Item & " ");
          S_Ovr : String :=
            Overwrite (Source,
                       Before,
                       New_Item);
          S_Del : String :=
            Trim (Delete (Source,
                          Before,
                          Before + Pattern'Length - 1),
                  Ada.Strings.Right);
       begin
          Insert (S_Ins_In,
                  Before,
                  New_Item,
                  Right);

          Overwrite (S_Ovr_In,
                     Before,
                     New_Item,
                     Right);

          Delete (S_Del_In,
                  Before,
                  Before + Pattern'Length - 1);

          Put_Line ("Original:  '"
                    & Source & "'");

          Put_Line ("Insert:    '"
                    & S_Ins  & "'");
          Put_Line ("Overwrite: '"
                    & S_Ovr  & "'");
          Put_Line ("Delete:    '"
                    & S_Del  & "'");

          Put_Line ("Insert    (in-place): '"
                    & S_Ins_In & "'");
          Put_Line ("Overwrite (in-place): '"
                    & S_Ovr_In & "'");
          Put_Line ("Delete    (in-place): '"
                    & S_Del_In & "'");
       end Display_Adapted_String;

       Idx : Natural;
    begin
       Idx := Index
         (Source  => S,
          Pattern => P);

       if Idx > 0 then
          Display_Adapted_String (S, Idx, N, P);
       end if;
    end Show_Adapted_Strings;

В этом примере мы ищем индекс подстроки :ada:`World` и выполняем операции с
этой подстрокой внутри внешней строки. Процедура :ada:`Display_Adapted_String`
использует обе версии операций. Для процедурной версии :ada:`Insert` и
:ada:`Overwrite` мы применяем усечение к правой стороне строки
(:ada:`Right`). Для процедуры :ada:`Delete` мы указываем диапазон подстроки,
которая заменяется пробелами. Для функциональной версии :ada:`Delete` мы
также вызываем :ada:`Trim`. Эта функция которая обрезает конечные пробелы.

Ограничение строк фиксированной длины
-------------------------------------

Использование строк фиксированной длины обычно достаточно хорошо для
строк, которые инициализируются при их объявлении. Однако, как видно
из предыдущего раздела, процедурные операции со строками вызывают
трудности при выполнении со строками фиксированной длины, поскольку
строки фиксированной длины представляют собой массивы символов. В
следующем примере показано, насколько громоздкой может быть
инициализация строк фиксированной длины, если она не выполняется в
объявлении:

.. code-block:: ada

    with Ada.Text_IO;         use Ada.Text_IO;

    procedure Show_Char_Array is
       S : String (1 .. 15);
       --  Strings are arrays of Character
    begin
       S := "Hello          ";
       --  Alternatively:
       --
       --  #1:
       --      S (1 .. 5)      := "Hello";
       --      S (6 .. S'Last) := (others => ' ');
       --
       --  #2:
       --      S := ('H', 'e', 'l', 'l', 'o',
       --            others => ' ');

       Put_Line ("String: " & S);
       Put_Line ("String Length: "
                 & Integer'Image (S'Length));
    end Show_Char_Array;

В этом случае мы не можем просто написать :ada:`S := "Hello"`, потому что
результирующий массив символов для константы :ada:`Hello` имеет длину,
отличную от длины строки :ada:`S` .Следовательно, нам необходимо включить
конечные пробелы, чтобы соответствовать длине :ada:`S`. Как показано в
примере, мы могли бы использовать точный диапазон для инициализации
(:ada:`S (1 .. 5)`) или использовать явный массив отдельных символов.

Когда строки инициализируются или обрабатываются во время выполнения,
обычно лучше использовать ограниченные или неограниченные строки.
Важной особенностью этих типов является то, что они не являются
массивами, поэтому описанные выше трудности не применяются. Начнем с
ограниченных строк.

Ограниченные строки
-------------------

Ограниченные строки определены в пакете
:ada:`Ada.Strings.Bounded.Generic_Bounded_Length`. Поскольку это общий пакет,
вам необходимо создать его экземпляр и установить максимальную длину
ограниченной строки. Затем вы можете объявить ограниченные строки типа
:ada:`Bounded_String`.

Строки как ограниченной, так и фиксированной длины имеют максимальную
длину, которую они могут удерживать. Однако ограниченные строки не
являются массивами, поэтому их инициализация во время выполнения
намного проще. Например:

.. code-block:: ada

    with Ada.Strings;         use Ada.Strings;
    with Ada.Strings.Bounded;
    with Ada.Text_IO;         use Ada.Text_IO;

    procedure Show_Bounded_String is
       package B_Str is new
         Ada.Strings.Bounded.Generic_Bounded_Length (Max => 15);
       use B_Str;

       S1, S2 : Bounded_String;

       procedure Display_String_Info (S : Bounded_String) is
       begin
          Put_Line ("String: " & To_String (S));
          Put_Line ("String Length: "
                    & Integer'Image (Length (S)));
          --  String:
          --          S'Length => ok
          --  Bounded_String:
          --          S'Length => compilation error:
          --                      bounded strings are
          --                      not arrays!

          Put_Line ("Max.   Length: "
                    & Integer'Image (Max_Length));
       end Display_String_Info;
    begin
       S1 := To_Bounded_String ("Hello");
       Display_String_Info (S1);

       S2 := To_Bounded_String ("Hello World");
       Display_String_Info (S2);

       S1 := To_Bounded_String
         ("Something longer to say here...",
          Right);
       Display_String_Info (S1);
    end Show_Bounded_String;

Используя ограниченные строки, мы можем легко назначить :ada:`S1` и :ada:`S2`
несколько раз во время выполнения. Мы используем функции
:ada:`To_Bounded_String` и :ada:`To_String` для преобразования
в соответствующем направлении между строками фиксированной длины и
ограниченными строками. Вызов :ada:`To_Bounded_String` вызывает исключение,
если длина входной строки больше максимальной емкости ограниченной строки.
Чтобы этого избежать, мы можем использовать параметр усечения (:ada:`Right`
в нашем примере).

Ограниченные строки не являются массивами, поэтому нельзя использовать
атрибут :ada:`'Length`, как это было для строк фиксированной длины. Вместо
этого вызывается функция :ada:`Length`, которая возвращает длину
ограниченной строки. Константа :ada:`Max_Length` представляет максимальную
длину ограниченной строки, заданную при создании экземпляра пакета.

После инициализации ограниченной строки мы можем манипулировать ею.
Например, мы можем добавить строку в ограниченную строку с помощью
:ada:`Append` или конкатенации ограниченных строк с помощью оператора
:ada:`&`. Вот так:

.. code-block:: ada

    with Ada.Strings;         use Ada.Strings;
    with Ada.Strings.Bounded;
    with Ada.Text_IO;         use Ada.Text_IO;

    procedure Show_Bounded_String_Op is
       package B_Str is new
         Ada.Strings.Bounded.Generic_Bounded_Length (Max => 30);
       use B_Str;

       S1, S2 : Bounded_String;
    begin
       S1 := To_Bounded_String ("Hello");
       --  Alternatively:
       --
       --  A := Null_Bounded_String & "Hello";

       Append (S1, " World");
       --  Alternatively: Append (A, " World", Right);

       Put_Line ("String: " & To_String (S1));

       S2 := To_Bounded_String ("Hello!");
       S1 := S1 & " " & S2;
       Put_Line ("String: " & To_String (S1));
    end Show_Bounded_String_Op;

Мы можем инициализировать ограниченную строку пустой строкой,
используя константу :ada:`Null_Bounded_String`. Кроме того, можно
использовать процедуру добавить (:ada:`Append`) и указать режим усечения,
как в случае с функцией :ada:`To_Bounded_String`.

.. _UnboundedStrings:

Неограниченные строки
---------------------

Неограниченные строки определяются в пакете :ada:`Ada.Strings.Unbounded`.
Это *не* универсальный пакет, поэтому нам не нужно создавать его экземпляр
перед использованием :ada:`Unbounded_String` типа. Как можно вспомнить из
предыдущего раздела, для ограниченных строк требуется экземпляр пакета.

Неограниченные строки похожи на ограниченные строки. Главное отличие
состоит в том, что они могут содержать строки любого размера и
подстраиваться в соответствии с входной строкой: если мы назначим,
например, 10-символьную строку неограниченной строке, а позже назначим
50-символьную строку, внутренние операции в контейнере обеспечат
выделение памяти для хранения новой строки. В большинстве случаев
разработчикам не нужно беспокоиться об этих операциях. Кроме того,
усечение не требуется.

Инициализация неограниченных строк очень похожа на ограниченные
строки. Рассмотрим пример:

.. code-block:: ada

    with Ada.Strings;           use Ada.Strings;
    with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
    with Ada.Text_IO;           use Ada.Text_IO;

    procedure Show_Unbounded_String is
       S1, S2 : Unbounded_String;

       procedure Display_String_Info (S : Unbounded_String) is
       begin
          Put_Line ("String: " & To_String (S));
          Put_Line ("String Length: "
                    & Integer'Image (Length (S)));
       end Display_String_Info;
    begin
       S1 := To_Unbounded_String ("Hello");
       --  Alternatively:
       --
       --  A := Null_Unbounded_String & "Hello";

       Display_String_Info (S1);

       S2 := To_Unbounded_String ("Hello World");
       Display_String_Info (S2);

       S1 := To_Unbounded_String ("Something longer to say here...");
       Display_String_Info (S1);
    end Show_Unbounded_String;

Как и ограниченные строки, мы можем назначать :ada:`S1` и :ada:`S2` несколько
раз во время выполнения и использовать функции :ada:`To_Unbounded_String` и
:ada:`To_String` для преобразования назад и вперед между строками
фиксированной длины и неограниченными строками. Однако в этом случае усечение
не нужно.

Как и для ограниченных строк, можно использовать функцию :ada:`Append` и
оператор :ada:`&` для неограниченных строк. Например:

.. code-block:: ada

    with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
    with Ada.Text_IO;           use Ada.Text_IO;

    procedure Show_Unbounded_String_Op is
       S1, S2 : Unbounded_String := Null_Unbounded_String;
    begin
       S1 := S1 & "Hello";
       S2 := S2 & "Hello!";

       Append (S1, " World");
       Put_Line ("String: " & To_String (S1));

       S1 := S1 & " " & S2;
       Put_Line ("String: " & To_String (S1));
    end Show_Unbounded_String_Op;
