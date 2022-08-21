.. _Containers:

Стандартная библиотека: Контейнеры
==================================

.. include:: ../../global.txt

В предыдущих главах мы использовали массивы в качестве стандартного
способа, который группирует нескольких объектов определенного типа
данных. Во многих случаях массивы достаточно хороши для
работы с групрой объектов. Однако бывают ситуации, которые
требуют большей гибкости и более совершенные операций. Для этих
случаев Ада предоставляет поддержку контейнеров - таких как векторы и
множества - в своей стандартной библиотеке.

Здесь мы представляем введение в контейнеры. Список всех контейнеров,
имеющихся в Аде, см. в :ref:`Приложении B <ContainersTable>`.

Векторы
-------

В следующих разделах мы представляем общий обзор векторов, включая
создание экземпляров, инициализацию и операции с элементами вектора
и самими векторами.

Создание экземпляра
~~~~~~~~~~~~~~~~~~~

Вот пример, показывающий настройку и объявление вектора :ada:`V`:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Standard_Library.Show_Vector_Inst

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

Контейнеры основаны на настраиваемых пакетах, поэтому мы не можем просто
объявить вектор, как если бы объявляли массив определенного типа:

.. code-block:: ada

    A : array (1 .. 10) of Integer;

Вместо этого нам сначала нужно создать экземпляр одного из этих
пакетов. Мы используем пакет контейнера (в данном случае
:ada:`Ada.Containers.Vectors`) и настраиваем
его, чтобы создать экземпляр настраиваемого пакета для желаемого
типа. Только затем мы сможем объявить вектор, используя тип из
созданного пакета. Такая настройка необходима для любого типа
контейнера стандартной библиотеки.

При настройки экземпляра :ada:`Integer_Vectors` мы указываем, что вектор
содержит элементы типа :ada:`Integer`, указывая его как :ada:`Element_Type`.
Подставляя для :ada:`Index_Type` тип :ada:`Natural`, мы указываем,
что допустимый диапазон индекса включает все натуральные числа. При желании мы
могли бы использовать более ограниченный диапазон.

Инициализация
~~~~~~~~~~~~~

Один из способов инициализации вектора - это конкатенация элементов. Мы
используем оператор :ada:`&`, как показано в следующем примере:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Vector_Init

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

Мы указываем :ada:`use Integer_Vectors`, чтобы получить прямой доступ к
типам и операциям из созданного пакета. Кроме того, пример знакомит нас с еще
одной операцией вектора: :ada:`Length`, она возвращает количество
элементов в векторе. Мы можем использовать точечную нотацию, потому что
:ada:`Vector` - это теговый тип, и это
позволяет нам писать, как :ada:`V.Length`, так и :ada:`Length (V)`.

Добавление элементов
~~~~~~~~~~~~~~~~~~~~

Вы добавляете элементы в вектор с помощью операций :ada:`Prepend` и
:ada:`Append`. Как следует из названий, эти операции добавляют элементы
в начало или конец вектора соответственно. Например:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Vector_Append

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

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Vector_First_Last_Element

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

Вы можете поменять местами элементы с помощью процедуры :ada:`Swap`,
передав ей *курсоры* на первый и последний элементы вектора
полученные функциями :ada:`First` и :ada:`Last`.
Курсор используются при переборе элементов контейнера и для
указания на отдельные его элементы.

С помощью этих операций мы можем написать код, чтобы поменять местами
первый и последний элементы вектора:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Vector_First_Last_Element

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

Самый простой способ перебрать элементы контейнера - использовать цикл
:ada:`for E of Our_Container`.
Это дает нам ссылку (:ada:`E`) на элемент в текущей позиции. Затем мы можем
использовать :ada:`E` непосредственно. Например:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Vector_Iteration

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

Этот код отображает каждый элемент вектора :ada:`V`.

Поскольку у нас есть ссылка, мы можем отобразить не только значение
элемента, но и изменить его. Например, мы могли бы легко записать
цикл, чтобы добавить единицу к каждому элементу вектора :ada:`V`:

.. code-block:: ada

    for E of V loop
       E := E + 1;
    end loop;

Мы также можем использовать индексы для доступа к элементам вектора.
Формат аналогичен циклу по элементам массива: мы используем в цикле
:ada:`for I in <range>`. Диапазон строим из :ada:`V.First_Index`
и :ada:`V.Last_Index`. Мы можем обратиться к текущему элементу, используя
операцию индексирования массива: :ada:`V (I)`.
Например:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Vector_Index_Iteration

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

Здесь, помимо вывода элементов вектора, мы также печатаем
каждый индекс :ada:`I`, точно так же, как то, что мы можем сделать для
индексов массива. Получить элемент можено, как с помощью
краткой формы :ada:`V (I)`, так и с помощью вызова функции
:ada:`V.Element (I)`, но не как :ada:`V.I`.

Как упоминалось в предыдущем разделе, курсоры можно использовать для
итерации по контейнерам. Для этого используйте функцию :ada:`Iterate`,
которая выдает курсоры для каждой позиции в векторе. Соответствующий цикл
имеет формат :ada:`for C in V.Iterate loop`. Как и в предыдущем примере с
использованием индексов, можно снова получить доступ к текущему элементу,
используя курсор в качестве индекса массива :ada:`V (C)`. Например:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Vector_Cursor_Iteration

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
цикл :ada:`while ... loop` для прохода по вектору. В этом случае мы должны
начать с курсора первого элемента (полученного с помощью вызова
:ada:`V.First`), а затем вызывать :ada:`Next (C)`, чтобы получить курсор для
последующих элементов. Функция :ada:`Next (C)` возвращает :ada:`No_Element`,
когда курсор достигает конца вектора. Используя курсор вы можете изменять
элементы вектора непосредственно.

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
*процедуры обработки*, которая получает отдельный элемент и выполняет над ним
некоторую работу. Вы можете вызвать :ada:`Update_Element`, передав
курсор и ссулку на процедуру обреботки. Например:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Vector_Update

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

Вы можете найти определенный элемент в векторе и получить его индекс.
Функция :ada:`Find_Index` вернет индекс первого элемента, соответствующего
искомому значению. В качестве альтернативы вы можете использовать
:ada:`Find`, чтобы получить курсор, ссылающийся на этот элемент. Например:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Find_Vector_Element

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

Как мы видели в предыдущем разделе, мы можем осуществлять прямой доступ к
элементам вектора используя индекс или курсор.
Но, если мы пытаемся получить доступ к элементу с недопустимым
индексом или курсором, будет возбуждено исключение,
поэтому мы сначала должны проверить, действителен ли
индекс или курсор, прежде чем использовать его для доступа к элементу.
В нашем примере :ada:`Find_Index` или :ada:`Find` могли не найти элемент
в векторе. Мы проверяем эту ситуацию, сравнивая индекс с :ada:`No_Index`
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

-  с помощью оператора конкатенации (:ada:`&`) при объявлении вектора, или

-  вызвав процедуры :ada:`Prepend` и :ada:`Append`.

Вам может потребоваться вставить элемент в определенное место, например,
перед определенным элементом в векторе. Вы делаете это, вызывая
:ada:`Insert`. Например:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Vector_Insert

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

Вы можете удалить элементы из вектора, передав соответствующий индекс
или курсор в процедуру удаления :ada:`Delete`. Если мы объединим это с
функциями :ada:`Find_Index` и :ada:`Find` из предыдущего раздела, мы
сможем написать программу, которая ищет определенный элемент и удаляет
его, если он найден:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Remove_Vector_Element

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

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Remove_Vector_Elements

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
13 использую курсор.

Другие операции
~~~~~~~~~~~~~~~

Мы видели некоторые операции с элементами вектора. Здесь мы продемонстрируем
операции с вектором в целом. Наиболее заметным является объединение
нескольких векторов, но мы также увидим такие операции с векторами,
как сортировка и слияния отсортированных массивов, которые рассматривают
вектор, как последовательность элементов, при
этом учитывают отношения элементов друг с другом.

Мы выполняем конкатенацию векторов с помощью оператора :ada:`&` для векторов.
Рассмотрим два вектора :ada:`V1` и :ada:`V2`. Мы можем объединить их,
выполнив :ada:`V := V1 & V2`. Результирующий вектор содержится в :ada:`V`.

Настраиваемый пакет :ada:`Generic_Sorting` является дочерним пакетом
:ada:`Ada.Containers.Vectors`. Он содержит операции
сортировки и объединения. Поскольку это настраиваемый пакет, вы не можете
использовать его непосредственно, но должны сначала настроить его. Чтобы
использовать эти операции с вектором целочисленных значений
(:ada:`Integer_Vectors` в нашем примере), вам необходимо настроить
пакет дочерний к :ada:`Integer_Vectors`.
Следующий пример поясняет, как это сделать.

После настройки :ada:`Generic_Sorting` мы делаем все операции
доступными с помощью спецификатора :ada:`use`.
Затем мы можем вызвать :ada:`Sort`, чтобы отсортировать
вектор, и :ada:`Merge`, чтобы объединить один вектор с другим.

В следующем примере представлен код, который работает тремя векторами
(:ada:`V1`, :ada:`V2`, :ada:`V3`) и использует операций конкатенации,
сортировки и слияния:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Vector_Ops

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

Множества это другим вид контейнеров. В то
время как векторы позволяют вставлять дублирующиеся элементы, множества
гарантируют, что дублированных элементов не будет.

В следующих разделах мы рассмотрим операции, которые вы можете
выполнять с множествами. Однако, поскольку многие операции с векторами
аналогичны тем, которые используются для множеств, мы рассмотрим их
здесь лишь кратко. Пожалуйста, обратитесь к разделу о векторах для
более подробного обсуждения.

Инициализация и итерация
~~~~~~~~~~~~~~~~~~~~~~~~

Чтобы инициализировать множество, вы можете вызвать процедуру :ada:`Insert`.
Делая это, вы должны убедиться, что не вставляются повторяющиеся
элементы: если вы попытаетесь вставить дубликат, вы получите
исключение. Если вы не уверены, что нет дубликатов, вы можете
воспользоваться другими вариантами:

-  версия :ada:`Insert`, которая возвращает логическое значение, указывающее,
   была ли вставка успешной;

-  процедура :ada:`Include`, которая молча игнорирует любую попытку вставить
   повторяющийся элемент.

Чтобы перебрать множество, вы можете использовать цикл :ada:`for E of S`,
аналогично векторам. Вы получаете ссылку на элемент в множестве.

Посмотрим на пример:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Set_Init

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

-  :ada:`Contains` и :ada:`Find`, чтобы проверить наличие элементов.

Чтобы удалить элементы, вы вызываете процедуру :ada:`Delete`. Однако,
аналогично описанной выше процедуре :ada:`Insert`, :ada:`Delete` возбуждает
исключение, если элемент, подлежащий удалению, отсутствует в множестве. Если
элемент может отсутствовать в момент удаления и вам не нужна проверка, то вы
можете вызвать процедуру :ada:`Exclude`, которая молча
игнорирует любую попытку удалить несуществующий элемент.

Функция :ada:`Contains` возвращает логическое значение Boolean, указывающее,
содержится ли значение в множестве. :ada:`Find` также ищет элемент в множестве,
но возвращает курсор на элемент или :ada:`No_Element`, если элемент не
существует. Вы можете использовать любую из этих функций для проверки
наличия элементов в множестве.

Давайте рассмотрим пример, в котором используются эти операции:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Set_Element_Ops

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

В дополнение к упорядоченным множествам, используемым в приведенных выше
примерах, стандартная библиотека также предлагает хешированные множества.
Справочное руководство требует следующей средней сложности каждой
операции:

.. |LOGN_2| replace:: (log N)\ :sup:`2`

+------------------------+-------------------------+---------------------+
| Операции               | :ada:`Ordered_Sets`     | :ada:`Hashed_Sets`  |
+========================+=========================+=====================+
| -  Insert              | O(|LOGN_2|)             | O(log N)            |
| -  Include             | или лучше               |                     |
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
элементами множествоа. Но Ада также предоставляет типичные операции над
множествами: объединение, пересечение, разность и симметричная
разность. В отличие от некоторых векторных операций, которые мы видели
раньше (например, слияния - :ada:`Merge`), здесь вы можете использовать
общепринятые операторы, такие как :ada:`-`. В следующей таблице перечислены
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
| Симметричная разность   | :ada:`xor`                     |
+-------------------------+--------------------------------+

В следующем примере используются эти операторы:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Set_Ops

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

Отображения для неопределенных типов
------------------------------------

В предыдущих разделах были представлены контейнеры для элементов
определенных типов. Хотя большинство примеров в этих разделах
использовали целочисленный тип :ada:`Integer` как тип элемента контейнера,
контейнеры также могут использоваться с неопределенными типами,
примером которых является тип :ada:`String`. Однако неопределенные типы
требуют другого вида контейнеров, разработанных специально для них.

Мы также изучим другой класс контейнеров: отображения. Они связывают
ключ с определенным значением. Примером отображения является связь «один к
одному» между человеком и его возрастом. Если мы считаем имя человека
ключевым, то значение - возраст человека.

Хэшированные отображения
~~~~~~~~~~~~~~~~~~~~~~~~

Хэшированные отображения - это отображения, которые используют хэш
ключа. Сам хэш вычисляется с помощью предоставленной вами функции.

.. admonition:: На других языках

    Хэшированные отображения похожи на словари в Python и хэши в Perl. Одно из
    основных отличий заключается в том, что эти скриптовые языки позволяют
    использовать разные типы для значений, содержащихся в одном отображении, в
    то время как в Аде, тип ключа и тип значение указываются в
    настройке пакета и остаются постоянными для этого конкретного отображения.
    У вас не может быть отображения, содержащего два элемента
    или два ключа разног типов. Если вы хотите использовать несколько типов, вы
    должны создать разные отображения для каждого и использовать только одино
    из них.

При создании настройке хешированного отображения
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

Вы добавляете элементы в хешированное отображение, вызывая :ada:`Insert`. Если
элемент уже содержится в отображении :ada:`M`, вы можете получить к нему доступ
непосредственно, используя его ключ.
Например, вы можете изменить значение элемента,
написав :ada:`M ("My_Key") := 10`. Если ключ не найден, возбуждается исключение.
Чтобы проверить, доступен ли ключ, используйте функцию :ada:`Contains` (как
мы видели выше в разделе о множествоах).

Посмотрим на пример:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Hashed_Map

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

Упорядоченные отображения
~~~~~~~~~~~~~~~~~~~~~~~~~

Упорядоченные отображения имеют много общих черт с хэшированными
отображениями. Основными отличиями являются:

-  Хэш-функция не нужна. Вместо этого вы должны предоставить
   функцию сравнения (:ada:`<` operator), которую упорядоченное
   отображение будет использовать для сравнения элементов и
   обеспечения быстрого доступа (сложность O(log N)), используя
   двоичный поиск.

   -  Если тип, указанный в :ada:`Key_Type`, имеет стандартный оператор
      :ada:`<`, вы можете использовать его аналогично тому, как мы это
      делали для :ada:`Equivalent_Keys` выше для хэшированных отображений.

Давайте посмотрим на пример:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Ordered_Map

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
отображений имеют много общих операций, нам не нужно было вносить
существенные изменения, когда мы изменили наш пример, чтобы
использовать упорядоченные отображения вместо хешированных. Основное
различие видно, когда мы запускаем примеры: вывод для хешированных отображений
обычно неупорядочен, но вывод для упорядоченных отображений всегда упорядочен,
как следует из его имени.

Сложность
~~~~~~~~~

Хэшированные отображения, как правило, являются самой быстрой структурой
данных, доступной в Аде, если необходимо связать неоднородные ключи со
значениями и быстро находить их. В большинстве случаев они немного
быстрее упорядоченных отображений. Так что, если вам не важен порядок,
используйте хэшированные отображения.

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
