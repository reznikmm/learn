.. _Containers:

Стандартная библиотека: Контейнеры
==================================

.. include:: ../../global.txt

В предыдущих главах мы использовали массивы в качестве стандартного
способа, который группирует нескольких объектов определенного типа
данных. Во многих случаях массивы достаточно хороши для
манипулирования этими объектами. Однако бывают ситуации, которые
требуют большей гибкости и более продвинутых операций. Для этих
случаев Ada предоставляет поддержку контейнеров - таких как векторы и
наборы - в своей стандартной библиотеке.

Здесь мы представляем введение в контейнеры. Список всех контейнеров,
имеющихся в Ada, см. в :ref:`Приложении B <ContainersTable>`.

Векторы
-------

В следующих разделах мы представляем общий обзор векторов, включая
создание экземпляров, инициализацию и операции с векторными элементами
и векторами.

Создание экземпляра
~~~~~~~~~~~~~~~~~~~

Вот пример, показывающий создание и объявление вектора :ada:`V`:

.. code-block:: ada

    with Ada.Containers.Vectors;

    procedure Show_Vector_Inst is

       package Integer_Vectors is new
         Ada.Containers.Vectors
           (Index_Type   => Natural,
            Element_Type => Integer);

       V : Integer_Vectors.Vector;
    begin
       null;
    end Show_Vector_Inst;

Контейнеры основаны на пакетах generic, поэтому мы не можем просто
объявить вектор, как если бы объявляли массив определенного типа:

.. code-block:: ada

    A : array (1 .. 10) of Integer;

Вместо этого нам сначала нужно создать экземпляр одного из этих
пакетов. Мы используем пакет контейнера (в данном случае
:ada:`Ada.Containers.Vectors`) и создаем
его экземпляр, чтобы создать экземпляр generic пакета для желаемого
типа. Только тогда мы сможем объявить вектор, используя тип из
созданного пакета. Этот экземпляр необходимо создавать для любого типа
контейнера из стандартной библиотеки.

При создании экземпляра :ada:`Integer_Vectors` мы указываем, что вектор
содержит элементы типа :ada:`Integer`, указывая его как :ada:`Element_Type`.
Устанавливая для :ada:`Index_Type` значение :ada:`Natural`, мы указываем,
что допустимый диапазон включает все натуральные числа. При желании мы
могли бы использовать более ограниченный диапазон.

Инициализация
~~~~~~~~~~~~~

Один из способов инициализации вектора - это объединение элементов. Мы
используем оператор :ada:`&`, как показано в следующем примере:

.. code-block:: ada

    with Ada.Containers; use Ada.Containers;
    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Vector_Init is

       package Integer_Vectors is new
         Ada.Containers.Vectors
           (Index_Type   => Natural,
            Element_Type => Integer);

       use Integer_Vectors;

       V : Vector := 20 & 10 & 0 & 13;
    begin
       Put_Line ("Vector has "
                 & Count_Type'Image (V.Length)
                 & " elements");
    end Show_Vector_Init;

Мы указываем :ada:`use Integer_Vectors`, поэтому у нас есть прямой доступ к
типам и операциям из созданного пакета. Кроме того, в примере вводится еще
одна операция с вектором: :ada:`Length`, которая извлекает количество
элементов в векторе. Мы можем использовать точечную нотацию, потому что
:ada:`Vector` - это тегированный тип, что
позволяет нам писать либо :ada:`V.Length`, либо :ada:`Length (V)`.

Добавление элементов - Prepend и Append
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Вы добавляете элементы в вектор с помощью операций :ada:`Prepend` и
:ada:`Append`. Как следует из названий, эти операции добавляют элементы
в начало или конец вектора соответственно. Например:

.. code-block:: ada

    with Ada.Containers; use Ada.Containers;
    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Vector_Append is

       package Integer_Vectors is new
         Ada.Containers.Vectors
           (Index_Type   => Natural,
            Element_Type => Integer);

       use Integer_Vectors;

       V : Vector;
    begin
       Put_Line ("Appending some elements to the vector...");
       V.Append (20);
       V.Append (10);
       V.Append (0);
       V.Append (13);
       Put_Line ("Finished appending.");

       Put_Line ("Prepending some elements to the vector...");
       V.Prepend (30);
       V.Prepend (40);
       V.Prepend (100);
       Put_Line ("Finished prepending.");

       Put_Line ("Vector has "
                 & Count_Type'Image (V.Length)
                 & " elements");
    end Show_Vector_Append;

В этом примере элементы помещаются в вектор в следующей
последовательности: (100, 40, 30, 20, 10, 0, 13).

Справочное руководство указывает, что сложность наихудшего случая
должна быть:

-  O(log N) для операции :ada:`Append` и

-  O(N log N) для операции :ada:`Prepend`.

Доступ к первому и последнему элементам
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Мы получаем доступ к первому и последнему элементам вектора с помощью
функций :ada:`First_Element` и :ada:`Last_Element`. Например:

.. code-block:: ada

    with Ada.Containers; use Ada.Containers;
    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Vector_First_Last_Element is

       package Integer_Vectors is new
         Ada.Containers.Vectors
           (Index_Type   => Natural,
            Element_Type => Integer);

       use Integer_Vectors;

       function Img (I : Integer)    return String
         renames Integer'Image;
       function Img (I : Count_Type) return String
         renames Count_Type'Image;

       V : Vector := 20 & 10 & 0 & 13;
    begin
       Put_Line ("Vector has "
                 & Img (V.Length)
                 & " elements");

       --  Using V.First_Element to
       --  retrieve first element
       Put_Line ("First element is "
                 & Img (V.First_Element));

       --  Using V.Last_Element to
       --  retrieve last element
       Put_Line ("Last element is "
                 & Img (V.Last_Element));
    end Show_Vector_First_Last_Element;

Вы можете поменять местами элементы, вызвав процедуру :ada:`Swap` и получив
ссылку (*курсор*) на первый и последний элементы вектора, вызвав
:ada:`First` и :ada:`Last`.
Курсор позволяет нам перебирать контейнер и обрабатывать отдельные
элементы из него.

С помощью этих операций мы можем написать код, чтобы поменять местами
первый и последний элементы вектора:

.. code-block:: ada

    with Ada.Containers; use Ada.Containers;
    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Vector_First_Last_Element is

       package Integer_Vectors is new
         Ada.Containers.Vectors
           (Index_Type   => Natural,
            Element_Type => Integer);

       use Integer_Vectors;

       function Img (I : Integer) return String
         renames Integer'Image;

       V : Vector := 20 & 10 & 0 & 13;
    begin
       --  We use V.First and V.Last to retrieve
       --  cursor for first and last elements.
       --  We use V.Swap to swap elements.
       V.Swap (V.First, V.Last);

       Put_Line ("First element is now "
                 & Img (V.First_Element));
       Put_Line ("Last element is now "
                 & Img (V.Last_Element));
    end Show_Vector_First_Last_Element;

Итерация
~~~~~~~~

Самый простой способ перебрать контейнер - использовать цикл
:ada:`for E of Our_Container`.
Это дает нам ссылку (:ada:`E`) на элемент в текущей позиции. Затем мы можем
напрямую использовать :ada:`E`. Например:

.. code-block:: ada

    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Vector_Iteration is

       package Integer_Vectors is new
         Ada.Containers.Vectors
           (Index_Type   => Natural,
            Element_Type => Integer);

       use Integer_Vectors;

       function Img (I : Integer) return String
         renames Integer'Image;

       V : Vector := 20 & 10 & 0 & 13;
    begin
       Put_Line ("Vector elements are: ");

       --
       --  Using for ... of loop to iterate:
       --
       for E of V loop
          Put_Line ("- " & Img (E));
       end loop;

    end Show_Vector_Iteration;

Этот код отображает каждый элемент из вектора :ada:`V`.

Поскольку нам дана ссылка, мы можем отобразить не только значение
элемента, но и изменить его. Например, мы могли бы легко записать
цикл, чтобы добавить один к каждому элементу вектора :ada:`V`:

.. code-block:: ada

    for E of V loop
       E := E + 1;
    end loop;

Мы также можем использовать индексы для доступа к векторным элементам.
Формат аналогичен циклу по элементам массива: мы используем в цикле
:ada:`for I in <range>`. Диапазон предоставляется :ada:`V.First_Index`
и :ada:`V.Last_Index`. Мы можем получить доступ к
текущему элементу, используя его как индекс массива: :ada:`V (I)`. 
Например:

.. code-block:: ada

    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Vector_Index_Iteration is

       package Integer_Vectors is new
         Ada.Containers.Vectors
           (Index_Type   => Natural,
            Element_Type => Integer);

       use Integer_Vectors;

       V : Vector := 20 & 10 & 0 & 13;
    begin
       Put_Line ("Vector elements are: ");

       --
       --  Using indices in a "for I in ..." loop
       --  to iterate:
       --
       for I in V.First_Index .. V.Last_Index loop
          --  Displaying current index I
          Put ("- ["
               & Extended_Index'Image (I)
               & "] ");

          Put (Integer'Image (V (I)));

          --  We could also use the V.Element (I)
          --  function to retrieve the element at
          --  the current index I

          New_Line;
       end loop;

    end Show_Vector_Index_Iteration;

Здесь, помимо отображения векторных элементов, мы также отображаем
каждый индекс :ada:`I`, точно так же, как то, что мы можем сделать для
индексов массива. Кроме того, мы можем получить доступ к элементу,
используя либо краткую форму :ada:`V (I)`, либо более длинную форму
:ada:`V.Element (I)`, но не :ada:`V.I`.

Как упоминалось в предыдущем разделе, курсоры можно использовать для
итерации по контейнерам. Для этого используйте функцию :ada:`Iterate`,
которая извлекает курсор для каждой позиции в векторе. Соответствующий цикл
имеет формат :ada:`for C in V.Iterate loop`. Как и в предыдущем примере с
использованием индексов, можно снова получить доступ к текущему элементу,
используя курсор в качестве индекса массива :ada:`V (C)`. Например:

.. code-block:: ada

    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Vector_Cursor_Iteration is

       package Integer_Vectors is new
         Ada.Containers.Vectors
           (Index_Type   => Natural,
            Element_Type => Integer);

       use Integer_Vectors;

       V : Vector := 20 & 10 & 0 & 13;
    begin
       Put_Line ("Vector elements are: ");

       --
       --  Use a cursor to iterate in a loop:
       --
       for C in V.Iterate loop
          --  Using To_Index function to retrieve
          --  the index for the cursor position
          Put ("- ["
               & Extended_Index'Image (To_Index (C))
               & "] ");

          Put (Integer'Image (V (C)));

          --  We could use Element (C) to retrieve
          --  the vector element for the cursor
          --  position

          New_Line;
       end loop;

       --  Alternatively, we could iterate with a
       --  while-loop:
       --
       --  declare
       --     C : Cursor := V.First;
       --  begin
       --     while C /= No_Element loop
       --        some processing here...
       --
       --        C := Next (C);
       --     end loop;
       --  end;

    end Show_Vector_Cursor_Iteration;

Мы также могли бы использовать более длинную форму :ada:`Element (C)`, вместо
:ada:`V (C)`, для доступа к элементу в цикле. В этом примере мы используем
функцию :ada:`To_Index` для получения индекса, соответствующего текущему
курсору.

Как показано в комментариях после цикла, мы также можем использовать
цикл :ada:`while ... loop` для перебора вектора. В этом случае мы должны
начать с курсора для первого элемента (полученного путем вызова
:ada:`V.First`), а затем вызвать :ada:`Next (C)`, чтобы получить курсор для
последующих элементов. :ada:`Next (C)` возвращает :ada:`No_Element`, когда
курсор достигает конца вектора. Вы можете напрямую изменять элементы,
используя ссылку.

Вот как это выглядит при использовании, как индексов, так и курсоров:

.. code-block:: ada

    --  Modify vector elements using index
    for I in V.First_Index .. V.Last_Index loop
       V (I) := V (I) + 1;
    end loop;

    --  Modify vector elements using cursor
    for C in V.Iterate loop
       V (C) := V (C) + 1;
    end loop;

Справочное руководство требует, чтобы сложность доступа к элементу в
наихудшем случае составляла O (log N).

Другой способ изменения элементов вектора - использование
*процедуры обработки*, которая берет отдельный элемент и выполняет над ним
некоторую обработку. Вы можете вызвать :ada:`Update_Element` и передать как
курсор, так и доступ к процедуре процесса. Например:

.. code-block:: ada

    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Vector_Update is

       package Integer_Vectors is new
         Ada.Containers.Vectors
           (Index_Type   => Natural,
            Element_Type => Integer);

       use Integer_Vectors;

       procedure Add_One (I : in out Integer) is
       begin
          I := I + 1;
       end Add_One;

       V : Vector := 20 & 10 & 12;
    begin
       --
       --  Use V.Update_Element to process elements
       --
       for C in V.Iterate loop
          V.Update_Element (C, Add_One'Access);
       end loop;

    end Show_Vector_Update;

Поиск и изменение элементов
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Вы можете найти определенный элемент в векторе, получив его индекс.
:ada:`Find_Index` извлекает индекс первого элемента, соответствующего
искомому значению. В качестве альтернативы вы можете использовать
:ada:`Find`, чтобы получить курсор, ссылающийся на этот элемент. Например:

.. code-block:: ada

    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Find_Vector_Element is

       package Integer_Vectors is new
         Ada.Containers.Vectors
           (Index_Type   => Natural,
            Element_Type => Integer);

       use Integer_Vectors;

       V : Vector := 20 & 10 & 0 & 13;
       Idx : Extended_Index;
       C   : Cursor;
    begin
       --  Using Find_Index to retrieve the index
       --  of element with value 10
       Idx := V.Find_Index (10);
       Put_Line ("Index of element with value 10 is "
                 & Extended_Index'Image (Idx));

       --  Using Find to retrieve the cursor for
       --  the element with value 13
       C   := V.Find (13);
       Idx := To_Index (C);
       Put_Line ("Index of element with value 13 is "
                 & Extended_Index'Image (Idx));
    end Show_Find_Vector_Element;

Как мы видели в предыдущем разделе, мы можем получить прямой доступ к
векторным элементам с помощью индекса или курсора. Однако возникает
исключение, если мы пытаемся получить доступ к элементу с недопустимым
индексом или курсором, поэтому мы должны проверить, действителен ли
индекс или курсор, прежде чем использовать его для доступа к элементу.
В нашем примере :ada:`Find_Index` или :ada:`Find` могли не найти элемент
в векторе. Мы проверяем эту возможность, сравнивая индекс с :ada:`No_Index`
или курсора с :ada:`No_Element`. Например:

.. code-block:: ada

    --  Modify vector element using index
    if Idx /= No_Index then
       V (Idx) := 11;
    end if;

    --  Modify vector element using cursor
    if C /= No_Element then
       V (C) := 14;
    end if;

Вместо того, чтобы писать :ada:`V (C) := 14`, мы могли бы использовать более
длинную форму :ada:`V.Replace_Element (C, 14)`.

Вставка элементов
~~~~~~~~~~~~~~~~~

В предыдущих разделах мы видели примеры того, как добавлять элементы в
вектор:

-  с помощью оператора конкатенации (:ada:`&`) в объявлении вектора, или

-  вызов процедур :ada:`Prepend` и :ada:`Append`.

Вы можете захотеть вставить элемент в определенную позицию, например,
перед определенным элементом в векторе. Вы делаете это, вызывая
:ada:`Insert`. Например:

.. code-block:: ada

    with Ada.Containers; use Ada.Containers;
    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Vector_Insert is

       package Integer_Vectors is new
         Ada.Containers.Vectors
           (Index_Type   => Natural,
            Element_Type => Integer);

       use Integer_Vectors;

       procedure Show_Elements (V : Vector) is
       begin
          New_Line;
          Put_Line ("Vector has "
                    & Count_Type'Image (V.Length)
                    & " elements");

          if not V.Is_Empty then
             Put_Line ("Vector elements are: ");
             for E of V loop
                Put_Line ("- " & Integer'Image (E));
             end loop;
          end if;
       end Show_Elements;

       V : Vector := 20 & 10 & 12;
       C : Cursor;
    begin
       Show_Elements (V);

       New_Line;
       Put_Line ("Adding element with value 9 (before 10)...");

       --
       --  Using V.Insert to insert the element
       --  into the vector
       --
       C := V.Find (10);
       if C /= No_Element then
          V.Insert (C, 9);
       end if;

       Show_Elements (V);

    end Show_Vector_Insert;

В этом примере мы ищем элемент со значением 10. Если он найден, перед
ним вставляется элемент со значением 9.

Удаление элементов
~~~~~~~~~~~~~~~~~~

Вы можете удалить элементы из вектора, передав действительный индекс
или курсор в процедуру удаления :ada:`Delete`. Если мы объединим это с
функциями :ada:`Find_Index` и :ada:`Find` из предыдущего раздела, мы
сможем написать программу, которая ищет определенный элемент и удаляет
его, если он найден:

.. code-block:: ada

    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Remove_Vector_Element is
       package Integer_Vectors is new
         Ada.Containers.Vectors
           (Index_Type   => Natural,
            Element_Type => Integer);

       use Integer_Vectors;

       V : Vector := 20 & 10 & 0 & 13 & 10 & 13;
       Idx : Extended_Index;
       C   : Cursor;
    begin
       --  Use Find_Index to retrieve index of
       --  the element with value 10
       Idx := V.Find_Index (10);

       --  Checking whether index is valid
       if Idx /= No_Index then
          --  Removing element using V.Delete
          V.Delete (Idx);
       end if;

       --  Use Find to retrieve cursor for
       --  the element with value 13
       C := V.Find (13);

       --  Check whether index is valid
       if C /= No_Element then
          --  Remove element using V.Delete
          V.Delete (C);
       end if;

    end Show_Remove_Vector_Element;

Мы можем расширить этот подход, чтобы удалить все элементы,
соответствующие определенному значению. Нам просто нужно продолжать
поиск элемента в цикле, пока мы не получим недопустимый индекс или
курсор. Например:

.. code-block:: ada

    with Ada.Containers; use Ada.Containers;
    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Remove_Vector_Elements is

       package Integer_Vectors is new
         Ada.Containers.Vectors
           (Index_Type   => Natural,
            Element_Type => Integer);

       use Integer_Vectors;

       procedure Show_Elements (V : Vector) is
       begin
          New_Line;
          Put_Line ("Vector has "
                    & Count_Type'Image (V.Length)
                    & " elements");

          if not V.Is_Empty then
             Put_Line ("Vector elements are: ");
             for E of V loop
                Put_Line ("- " & Integer'Image (E));
             end loop;
          end if;
       end Show_Elements;

       V : Vector := 20 & 10 & 0 & 13 & 10 & 14 & 13;
    begin
       Show_Elements (V);

       --
       --  Remove elements using an index
       --
       declare
          E : constant Integer := 10;
          I : Extended_Index;
       begin
          New_Line;
          Put_Line ("Removing all elements with value of "
                    & Integer'Image (E) & "...");
          loop
             I := V.Find_Index (E);
             exit when I = No_Index;
             V.Delete (I);
          end loop;
       end;

       --
       --  Remove elements using a cursor
       --
       declare
          E : constant Integer := 13;
          C : Cursor;
       begin
          New_Line;
          Put_Line ("Removing all elements with value of "
                    & Integer'Image (E) & "...");
          loop
             C := V.Find (E);
             exit when C = No_Element;
             V.Delete (C);
          end loop;
       end;

       Show_Elements (V);
    end Show_Remove_Vector_Elements;

В этом примере мы удаляем из вектора все элементы со значением 10,
получая их индекс. Точно так же мы удаляем все элементы со значением
13, извлекая их курсор.

Другие операции
~~~~~~~~~~~~~~~

Мы видели некоторые операции с векторными элементами. Здесь мы увидим
операции с вектором в целом. Наиболее заметным является объединение
нескольких векторов, но мы также увидим операции с векторами, такие
как операции сортировки и сортировки слияния, которые рассматривают
вектор как последовательность элементов и работают с вектором, при
этом учитывают отношения элементов друг к другу.

Мы выполняем конкатенацию векторов с помощью оператора :ada:`&` для векторов.
Рассмотрим два вектора :ada:`V1` и :ada:`V2`. Мы можем объединить их,
выполнив :ada:`V := V1 & V2`. Результирующий вектор содержится в :ada:`V`.

Общий пакет :ada:`Generic_Sorting` является дочерним пакетом
:ada:`Ada.Containers.Vectors`. Он содержит операции
сортировки и объединения. Поскольку это общий пакет, вы не можете
использовать его напрямую, но должны создать его экземпляр. Чтобы
использовать эти операции с вектором целочисленных значений
(:ada:`Integer_Vectors` в нашем
примере), вам необходимо создать его непосредственно как дочерний
элемент :ada:`Integer_Vectors`. Следующий пример поясняет, как это сделать.

После создания экземпляра :ada:`Generic_Sorting` мы делаем все операции
доступными для нас с
помощью оператора :ada:`use`. Затем мы можем вызвать :ada:`Sort`, чтобы
отсортировать
вектор, и :ada:`Merge`, чтобы объединить один вектор с другим.

В следующем примере представлен код, который управляет тремя векторами
(:ada:`V1`, :ada:`V2`, :ada:`V3`) с помощью операций конкатенации,
сортировки и слияния:

.. code-block:: ada

    with Ada.Containers; use Ada.Containers;
    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Vector_Ops is

       package Integer_Vectors is new
         Ada.Containers.Vectors
           (Index_Type   => Natural,
            Element_Type => Integer);

       package Integer_Vectors_Sorting is new Integer_Vectors.Generic_Sorting;

       use Integer_Vectors;
       use Integer_Vectors_Sorting;

       procedure Show_Elements (V : Vector) is
       begin
          New_Line;
          Put_Line ("Vector has "
                    & Count_Type'Image (V.Length)
                    & " elements");

          if not V.Is_Empty then
             Put_Line ("Vector elements are: ");
             for E of V loop
                Put_Line ("- " & Integer'Image (E));
             end loop;
          end if;
       end Show_Elements;

       V, V1, V2, V3 : Vector;
    begin
       V1 := 10 & 12 & 18;
       V2 := 11 & 13 & 19;
       V3 := 15 & 19;

       New_Line;
       Put_Line ("---- V1 ----");
       Show_Elements (V1);

       New_Line;
       Put_Line ("---- V2 ----");
       Show_Elements (V2);

       New_Line;
       Put_Line ("---- V3 ----");
       Show_Elements (V3);

       New_Line;
       Put_Line ("Concatenating V1, V2 and V3 into V:");

       V := V1 & V2 & V3;

       Show_Elements (V);

       New_Line;
       Put_Line ("Sorting V:");

       Sort (V);

       Show_Elements (V);

       New_Line;
       Put_Line ("Merging V2 into V1:");

       Merge (V1, V2);

       Show_Elements (V1);

    end Show_Vector_Ops;

Справочное руководство требует, чтобы худшей сложностью вызова для
сортировки :ada:`Sort` было O(N\ sup:`2`) и средняя сложность должна
быть лучше, чем O(N\ sup:`2`).

Множества
---------

Наборы (или множества – set) являются другим классом контейнеров. В то
время как векторы позволяют вставлять дублированные элементы, наборы
гарантируют, что дублированных элементов не существует.

В следующих разделах мы рассмотрим операции, которые вы можете
выполнять с наборами. Однако, поскольку многие операции с векторами
аналогичны тем, которые используются для наборов, мы рассмотрим их
здесь более быстро. Пожалуйста, обратитесь к разделу о векторах для
более подробного обсуждения.

Инициализация и итерация
~~~~~~~~~~~~~~~~~~~~~~~~

Чтобы инициализировать набор, вы можете вызвать процедуру :ada:`Insert`.Если
вы это сделаете, вам нужно убедиться, что не вставляются повторяющиеся
элементы: если вы попытаетесь вставить дубликат, вы получите
исключение. Если у вас меньше контроля над вставляемыми элементами,
поэтому могут быть дубликаты, вы можете вместо этого использовать
другой вариант:

-  версия :ada:`Insert`, которая возвращает логическое значение, указывающее,
   была ли вставка успешной;

-  процедура :ada:`Include`, которая молча игнорирует любую попытку вставить
   дублированный элемент.

Чтобы перебрать набор, вы можете использовать цикл :ada:`for E of S`, как вы
видели для векторов. Это дает вам ссылку на каждый элемент в наборе.

Посмотрим на пример:

.. code-block:: ada

    with Ada.Containers; use Ada.Containers;
    with Ada.Containers.Ordered_Sets;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Set_Init is

       package Integer_Sets is new
         Ada.Containers.Ordered_Sets
           (Element_Type => Integer);

       use Integer_Sets;

       S : Set;
       --  Same as:  S : Integer_Sets.Set;
       C : Cursor;
       Ins : Boolean;
    begin
       S.Insert (20);
       S.Insert (10);
       S.Insert (0);
       S.Insert (13);

       --  Calling S.Insert(0) now would raise
       --  Constraint_Error because this element
       --  is already in the set. We instead call a
       --  version of Insert that doesn't raise an
       --  exception but instead returns a Boolean
       --  indicating the status

       S.Insert (0, C, Ins);
       if not Ins then
          Put_Line ("Inserting 0 into set was not successful");
       end if;

       --  We can also call S.Include instead
       --  If the element is already present,
       --  the set remains unchanged
       S.Include (0);
       S.Include (13);
       S.Include (14);

       Put_Line ("Set has "
                 & Count_Type'Image (S.Length)
                 & " elements");

       --
       --  Iterate over set using for .. of loop
       --
       Put_Line ("Elements:");
       for E of S loop
           Put_Line ("- " & Integer'Image (E));
       end loop;
    end Show_Set_Init;

Операции с элементами
~~~~~~~~~~~~~~~~~~~~~

В этом разделе мы кратко рассмотрим следующие операции над
множествами:

-  :ada:`Delete` и :ada:`Exclude`, чтобы удалить элементы;

-  :ada:`Contains` и :ada:`Find`, чтобы проверить существование элементов.

Чтобы удалить элементы, вы вызываете процедуру :ada:`Delete`. Однако,
аналогично описанной выше процедуре :ada:`Insert`, :ada:`Delete` вызывает
исключение, если элемент, подлежащий удалению, отсутствует в наборе. Когда
элемент может не существовать во время удаления и вы хотите разрешить этот
случай. Тогда вы можете вызвать процедуру :ada:`Exclude`, которая молча
игнорирует любую попытку удалить несуществующий элемент.

:ada:`Contains` возвращает логическое значение Boolean, указывающее,
содержится ли значение в наборе. :ada:`Find` также ищет элемент в наборе,
но возвращает курсор на элемент или :ada:`No_Element`, если элемент не
существует. Вы можете использовать любую из этих функций для поиска элементов
в наборе.

Давайте рассмотрим пример, в котором используются эти операции:

.. code-block:: ada

    with Ada.Containers; use Ada.Containers;
    with Ada.Containers.Ordered_Sets;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Set_Element_Ops is

       package Integer_Sets is new
         Ada.Containers.Ordered_Sets
           (Element_Type => Integer);

       use Integer_Sets;

       procedure Show_Elements (S : Set) is
       begin
          New_Line;
          Put_Line ("Set has "
                    & Count_Type'Image (S.Length)
                    & " elements");
          Put_Line ("Elements:");
          for E of S loop
             Put_Line ("- " & Integer'Image (E));
          end loop;
       end Show_Elements;

       S : Set;
    begin
       S.Insert (20);
       S.Insert (10);
       S.Insert (0);
       S.Insert (13);

       S.Delete (13);

       --  Calling S.Delete (13) again raises
       --  Constraint_Error because the element
       --  is no longer present in the set, so
       --  it can't be deleted. We can call
       --  V.Exclude instead:
       S.Exclude (13);

       if S.Contains (20) then
          Put_Line ("Found element 20 in set");
       end if;

       --  Alternatively, we could use S.Find
       --  instead of S.Contains
       if S.Find (0) /= No_Element then
          Put_Line ("Found element 0 in set");
       end if;

       Show_Elements (S);
    end Show_Set_Element_Ops;

В дополнение к упорядоченным наборам, используемым в приведенных выше
примерах, стандартная библиотека также предлагает хешированные наборы.
Справочное руководство требует следующей средней сложности каждой
операции:

.. |LOGN_2| replace:: (log N)\ :sup:`2`

+------------------------+-------------------------+---------------------+
| Операции               | :ada:`Ordered_Sets`     | :ada:`Hashed_Sets`  |
+========================+=========================+=====================+
| -  Insert              | O(|LOGN_2|)             | O(log N)            |
|                        | или лучше               |                     |
| -  Include             |                         |                     |
| -  Replace             |                         |                     |
| -  Delete              |                         |                     |
| -  Exclude             |                         |                     |
| -  Find                |                         |                     |
+------------------------+-------------------------+---------------------+
| Подпрограмма с         | O(1)                    | O(1)                |
| использованием курсора |                         |                     |
+------------------------+-------------------------+---------------------+

Другие операции
~~~~~~~~~~~~~~~

Предыдущие разделы в основном касались операций с отдельными
элементами набора. Но Ada также предоставляет типичные операции над
множеством: объединение, пересечение, разность и симметричное
различие. В отличие от некоторых векторных операций, которые мы видели
раньше (например, слияния - :ada:`Merge`), здесь вы можете использовать
встроенные операторы, такие как :ada:`-`. В следующей таблице перечислены
операции и связанный с ними оператор:

+-------------------------+--------------------------------+
| Операции над множеством | Оператор                       |
+=========================+================================+
| Объединение             | :ada:`or`                      |
+-------------------------+--------------------------------+
| Пересечение             | :ada:`and`                     |
+-------------------------+--------------------------------+
| Разность                | :ada:`-`                       |
+-------------------------+--------------------------------+
| Симметричное различие   | :ada:`xor`                     |
+-------------------------+--------------------------------+

В следующем примере используются эти операторы:

.. code-block:: ada

    with Ada.Containers; use Ada.Containers;
    with Ada.Containers.Ordered_Sets;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Set_Ops is

       package Integer_Sets is new
         Ada.Containers.Ordered_Sets
           (Element_Type => Integer);

       use Integer_Sets;

       procedure Show_Elements (S : Set) is
       begin
          Put_Line ("Elements:");
          for E of S loop
             Put_Line ("- " & Integer'Image (E));
          end loop;
       end Show_Elements;

       procedure Show_Op (S       : Set;
                          Op_Name : String) is
       begin
          New_Line;
          Put_Line (Op_Name
                    & "(set #1, set #2) has "
                    & Count_Type'Image (S.Length)
                    & " elements");
       end Show_Op;

       S1, S2, S3 : Set;
    begin
       S1.Insert (0);
       S1.Insert (10);
       S1.Insert (13);

       S2.Insert (0);
       S2.Insert (10);
       S2.Insert (14);

       S3.Insert (0);
       S3.Insert (10);

       New_Line;
       Put_Line ("---- Set #1 ----");
       Show_Elements (S1);

       New_Line;
       Put_Line ("---- Set #2 ----");
       Show_Elements (S2);

       New_Line;
       Put_Line ("---- Set #3 ----");
       Show_Elements (S3);

       New_Line;
       if S3.Is_Subset (S1) then
          Put_Line ("S3 is a subset of S1");
       else
          Put_Line ("S3 is not a subset of S1");
       end if;

       S3 := S1 and S2;
       Show_Op (S3, "Intersection");
       Show_Elements (S3);

       S3 := S1 or S2;
       Show_Op (S3, "Union");
       Show_Elements (S3);

       S3 := S1 - S2;
       Show_Op (S3, "Difference");
       Show_Elements (S3);

       S3 := S1 xor S2;
       Show_Op (S3, "Symmetric difference");
       Show_Elements (S3);

    end Show_Set_Ops;

Неопределенные карты
--------------------

В предыдущих разделах были представлены контейнеры для элементов
определенных типов. Хотя большинство примеров в этих разделах
представляли целочисленные типы :ada:`Integer` как тип элемента контейнеров,
контейнеры также могут использоваться с неопределенными типами,
примером которых является тип :ada:`String`. Однако неопределенные типы
требуют другого вида контейнеров, разработанных специально для них.

Мы также будем изучать другой класс контейнеров: карты. Они связывают
ключ с определенным значением. Примером карты является связь «один к
одному» между человеком и его возрастом. Если мы считаем имя человека
ключевым, то значение - возраст человека.

Хэшированные карты
~~~~~~~~~~~~~~~~~~

Хэшированные карты - это карты, которые используют хэш в качестве
ключа. Сам хэш вычисляется с помощью предоставленной вами функции.

.. admonition:: На других языках

    Хэшированные карты похожи на словари в Python и хэши в Perl. Одно из
    основных отличий заключается в том, что эти языки сценариев позволяют
    использовать разные типы для значений, содержащихся в одной карте, в
    то время как в Ada как тип ключа, так и значение указываются в
    экземпляре пакета и остаются постоянными для этой конкретной карты. У
    вас не может быть карты, на которой два элемента разных типов или два
    ключа разных типов. Если вы хотите использовать несколько типов, вы
    должны создать разные карты для каждого и использовать только один тип
    на каждой карте.

При создании экземпляра хешированной карты из
:ada:`Ada.Containers.Indefinite_Hashed_Maps` мы указываем следующие элементы:

-  :ada:`Key_Type`: тип ключа
-  :ada:`Element_Type`: тип элемента
-  :ada:`Hash`:ada:`Key_Type`
-  :ada:`Equivalent_Keys`: оператор равенства (например, :ada:`=`), который
   указывает, должны ли два ключа считаться равными.

   -  Если тип, указанный в :ada:`Key_Type`, имеет стандартный оператор, вы
      можете использовать его. В примере мы так и делаем. Мы указываем этот
      оператор как значение :ada:`Equivalent_Keys`.

В следующем примере мы будем использовать строку в качестве типа
ключа. Мы будем использовать функцию :ada:`Hash`, предоставляемую стандартной
библиотекой для строк (в пакете :ada:`Ada.Strings`), и стандартный оператор
равенства.

Вы добавляете элементы в хешированную карту, вызывая :ada:`Insert`. Если
элемент уже содержится в карте :ada:`M`, вы можете получить к нему доступ
напрямую, используя его ключ. Например, вы можете изменить значение элемента,
вызвав :ada:`M ("My_Key") := 10`. Если ключ не найден, возникает исключение.
Чтобы проверить, доступен ли ключ, используйте функцию :ada:`Contains` (как
мы видели выше в разделе о наборах).

Посмотрим на пример:

.. code-block:: ada

    with Ada.Containers.Indefinite_Hashed_Maps;
    with Ada.Strings.Hash;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Hashed_Map is

       package Integer_Hashed_Maps is new
         Ada.Containers.Indefinite_Hashed_Maps
           (Key_Type        => String,
            Element_Type    => Integer,
            Hash            => Ada.Strings.Hash,
            Equivalent_Keys => "=");

       use Integer_Hashed_Maps;

       M : Map;
       --  Same as:
       --
       --  M : Integer_Hashed_Maps.Map;
    begin
       M.Include ("Alice", 24);
       M.Include ("John",  40);
       M.Include ("Bob",   28);

       if M.Contains ("Alice") then
          Put_Line ("Alice's age is "
                    & Integer'Image (M ("Alice")));
       end if;

       --  Update Alice's age
       --  Key must already exist in M.
       --  Otherwise an exception is raised.
       M ("Alice") := 25;

       New_Line; Put_Line ("Name & Age:");
       for C in M.Iterate loop
          Put_Line (Key (C) & ": "
                    & Integer'Image (M (C)));
       end loop;

    end Show_Hashed_Map;

Упорядоченные карты
~~~~~~~~~~~~~~~~~~~

Ordered maps share many features with hashed maps. The main
differences are:

-  A hash function isn't needed. Instead, you must provide an ordering
   function (:ada:`<` operator), which the ordered map will use to order
   elements and allow fast access, O(log N), using a binary search.

   -  If the type specified in :ada:`Key_Type` has a standard :ada:`<`
      operator, you can use it in a similar way as we did for
      :ada:`Equivalent_Keys` above for hashed maps.

Let's see an example:

.. code-block:: ada

    with Ada.Containers.Indefinite_Ordered_Maps;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Ordered_Map is

       package Integer_Ordered_Maps is new
         Ada.Containers.Indefinite_Ordered_Maps
           (Key_Type        => String,
            Element_Type    => Integer);

       use Integer_Ordered_Maps;

       M : Map;
    begin
       M.Include ("Alice", 24);
       M.Include ("John",  40);
       M.Include ("Bob",   28);

       if M.Contains ("Alice") then
          Put_Line ("Alice's age is "
                    & Integer'Image (M ("Alice")));
       end if;

       --  Update Alice's age
       --  Key must already exist in M
       M ("Alice") := 25;

       New_Line; Put_Line ("Name & Age:");
       for C in M.Iterate loop
          Put_Line (Key (C) & ": "
                    & Integer'Image (M (C)));
       end loop;

    end Show_Ordered_Map;

Вы можете увидеть большое сходство между примерами, приведенными выше,
и примерами из предыдущего раздела. Фактически, поскольку оба типа
карт имеют много общих операций, нам не нужно было вносить
существенные изменения, когда мы изменили наш пример, чтобы
использовать упорядоченные карты вместо хешированных карт. Основное
различие видно, когда мы запускаем примеры: вывод хешированной карты
обычно неупорядочен, но вывод упорядоченной карты всегда упорядочен,
как следует из ее имени.

Сложность
~~~~~~~~~

Хэшированные карты, как правило, являются самой быстрой структурой
данных, доступной в Ada, если необходимо связать разнородные ключи со
значениями и быстро найти их. В большинстве случаев они немного
быстрее упорядоченных карт. Так что если вам не нужна упорядоченная
последовательность, используйте хэшированные карты.

Справочное руководство требует следующей средней сложности операций:

+------------------------+-------------------------+---------------------+
| Операции               | :ada:`Ordered_Maps`     | :ada:`Hashed_Maps`  |
+========================+=========================+=====================+
| -  Insert              | O(|LOGN_2|) or better   | O(log N)            |
| -  Include             |                         |                     |
| -  Replace             |                         |                     |
| -  Delete              |                         |                     |
| -  Exclude             |                         |                     |
| -  Find                |                         |                     |
+------------------------+-------------------------+---------------------+
| Подпрограмма с         | O(1)                    | O(1)                |
| использованием курсора |                         |                     |
+------------------------+-------------------------+---------------------+
