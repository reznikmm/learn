Модульное программирование
==========================

.. include:: ../../global.txt

До сих пор наши примеры были простыми автономными подпрограммами. Ada
полезна в этом отношении, поскольку она допускает произвольные
объявления в декларативной части. Таким образом, мы смогли объявить
наши типы и переменные в телах основных процедур.

Однако легко увидеть, что это не будет масштабироваться для реальных
приложений. Нам нужен лучший способ структурировать наши программы в
модульные и отдельные блоки.

Ada поощряет разделение программ на несколько пакетов и подпакетов,
предоставляя множество инструментов программисту в поисках идеально
организованной базы кода.

Пакеты
------

Вот пример объявления пакета в Ada:

.. code-block:: ada

    package Week is

       Mon : constant String := "Monday";
       Tue : constant String := "Tuesday";
       Wed : constant String := "Wednesday";
       Thu : constant String := "Thursday";
       Fri : constant String := "Friday";
       Sat : constant String := "Saturday";
       Sun : constant String := "Sunday";

    end Week;

И вот как вы его используете:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;
    with Week;
    --  References the Week package, and
    --  adds a dependency from Main to Week

    procedure Main is
    begin
       Put_Line ("First day of the week is "
                 & Week.Mon);
    end Main;

Пакеты позволяют сделать код модульным, разделяя программы на
семантически значимые единицы. Кроме того, отделение спецификации
пакета от его тела (которое мы увидим ниже) может сократить время
компиляции.

Хотя предложение :ada:`with` указывает на зависимость, в приведенном выше примере
можно увидеть, что по-прежнему необходимо префиксировать ссылку на
сущности из пакета Week по имени пакета. (Если бы мы включили
предложение :ada:`use Week`, то такой префикс был бы не нужен.)

При доступе к объектам из пакета используется точечная нотация :ada:`A.B`,
которая является той же нотацией, что и та, которая используется для
доступа к полям записей.

Предложение :ada:`with` может появляться *только* в начале блока компиляции (т. е.
перед зарезервированным словом, таким как :ada:`procedure`, которое отмечает
начало блока). Больше нигде это не разрешено. Это правило необходимо
только по методологическим соображениям: человек, читающий ваш код,
должен сразу видеть, от каких единиц зависит код.

.. admonition:: На других языках

    Пакеты похожи на файлы заголовков в C / C ++, но семантически сильно
    отличаются от них.

     - Первое и самое важное отличие состоит в том, что пакеты представляют
       собой механизм уровня языка. Это контрастирует с заголовочным файлом :c:`#include`
       ,который является функцией препроцессора C.

     - Непосредственным следствием этого является то, что конструкция :ada:`with`
       является семантическим механизмом включения, а не механизмом включения
       текста. Следовательно, когда вы работаете с пакетом указывая :ada:`with`,
       вы говорите компилятору: «Я зависим от этой семантической единицы», а не
       «включите сюда эту кучу текста».

     - Таким образом, действие пакета не зависит от того, откуда на него ссылается
       :ada:`with`. Сравните это с C/C++, где значение включенного текста зависит от
       контекста, в котором появляется :c:`#include`.

       Это позволяет повысить эффективность компиляции / перекомпиляции. Это
       также позволяет таким инструментам, как IDE, иметь правильную
       информацию о семантике программы. В свою очередь, это позволяет
       улучшить инструменты в целом и сделать код более анализируемым даже
       людьми.

    Важным преимуществом Ada с :ada:`with` по сравнению с :c:`#include` является то, что он не
    имеет состояния. Порядок предложений :ada:`with` и :ada:`use` не имеет значения и может
    быть изменен без побочных эффектов.

.. admonition:: В наборе инструментов GNAT

    Стандарт языка Ada не предусматривает каких-либо особых отношений
    между исходными файлами и пакетами; например, теоретически вы можете
    поместить весь свой код в один файл или использовать свои собственные
    соглашения об именовании файлов. На практике, однако, реализация будет
    иметь конкретные правила. С помощью GNAT каждый модуль компиляции
    верхнего уровня должен быть помещен в отдельный файл. В приведенном
    выше примере пакет :ada:`Week` будет находиться в файле ``.ads`` (для спецификации Ada),
    а основная процедура :ada:`Main` ‑ в файле ``.adb`` (для тела Ada).

Использование пакета
--------------------

Как мы видели выше, предложение :ada:`with` указывает на зависимость от другого
пакета. Тем не менее, каждая ссылка на объект, поступающий из пакета :ada:`Week`
,должна иметь префикс полного имени пакета. Можно сделать каждый
объект пакета видимым непосредственно в текущей области с помощью
предложения :ada:`use`.

Фактически, мы использовали предложение :ada:`use` почти с самого начала этого
руководства.

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;
    --                ^ Make every entity of the
    --                  Ada.Text_IO package
    --                  directly visible.
    with Week;

    procedure Main is
       use Week;
       --  Make every entity of the Week
       --  package directly visible.
    begin
       Put_Line ("First day of the week is " & Mon);
    end Main;

Как вы можете видеть в приведенном выше примере:

-  :ada:`Put_Line` ‑ это подпрограмма из пакета :ada:`Ada.Text_IO`. Мы можем ссылаться на него напрямую,
   потому что мы использовали пакет с :ada:`use` в верхней части основного модуля :ada:`Main`
   .

-  В отличие от предложений :ada:`with`, предложение :ada:`use` может быть помещено либо в
   прелюдию, либо в любую декларативную область. В последнем случае
   использования :ada:`use` повлияет на содержащуюся в нем лексическую область
   видимости.

Тело пакета
-----------

В приведенном выше простом примере пакет :ada:`Week` содержит только объявления и
не содержит реализацию пакета. Это не ошибка: в спецификации пакета,
которая проиллюстрирована выше, нельзя объявлять реализацию.
Реализация должны быть в теле пакета.

.. code-block:: ada

    package Operations is

       --  Declaration
       function Increment_By
         (I    : Integer;
          Incr : Integer := 0) return Integer;

       function Get_Increment_Value return Integer;

    end Operations;

    package body Operations is

       Last_Increment : Integer := 1;

       function Increment_By
         (I    : Integer;
          Incr : Integer := 0) return Integer is
       begin
          if Incr /= 0 then
             Last_Increment := Incr;
          end if;

          return I + Last_Increment;
       end Increment_By;

       function Get_Increment_Value return Integer is
       begin
          return Last_Increment;
       end Get_Increment_Value;

    end Operations;

Здесь мы видим, что тело функции :ada:`Increment_By` должно быть объявлено в теле. По
совпадению, введение тела позволяет поместить :ada:`Last_Increment` переменную в тело, и
сделать их недоступными для пользователя пакета :ada:`Operations`, обеспечивая первую
форму инкапсуляции.

Это работает, поскольку объекты, объявленные в теле, видны *только* в
теле.

В этом примере показано, как :ada:`Last_Increment` используется косвенно:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;
    with Operations;

    procedure Main is
       use Operations;

       I : Integer := 0;
       R : Integer;

       procedure Display_Update_Values is
          Incr : constant Integer := Get_Increment_Value;
       begin
          Put_Line (Integer'Image (I)
                    & " incremented by "
                    & Integer'Image (Incr)
                    & " is "
                    & Integer'Image (R));
          I := R;
       end Display_Update_Values;
    begin
       R := Increment_By (I);
       Display_Update_Values;
       R := Increment_By (I);
       Display_Update_Values;

       R := Increment_By (I, 5);
       Display_Update_Values;
       R := Increment_By (I);
       Display_Update_Values;

       R := Increment_By (I, 10);
       Display_Update_Values;
       R := Increment_By (I);
       Display_Update_Values;
    end Main;

.. _ChildPackages:

Дочерние пакеты
---------------

Пакеты можно использовать для создания иерархий. Мы достигаем этого с
помощью дочерних пакетов, которые расширяют функциональность
родительского пакета. Одним из примеров дочернего пакета, который мы
использовали до сих пор, является пакет :ada:`Ada.Text_IO`. Здесь родительский пакет
называется :ada:`Ada`, а дочерний пакет называется :ada:`Text_IO`. В предыдущих примерах мы
использовали процедуру :ada:`Put_Line` из дочернего пакета :ada:`Text_IO`.

.. admonition:: Важное замечание

    Ada также поддерживает вложенные пакеты. Однако, поскольку их
    использование может быть более сложным, рекомендуется использовать
    дочерние пакеты. Вложенные пакеты будут рассмотрены в расширенном
    курсе.

Давайте начнем обсуждение дочерних пакетов с нашего предыдущего пакета
:ada:`Week`:

.. code-block:: ada

    package Week is

       Mon : constant String := "Monday";
       Tue : constant String := "Tuesday";
       Wed : constant String := "Wednesday";
       Thu : constant String := "Thursday";
       Fri : constant String := "Friday";
       Sat : constant String := "Saturday";
       Sun : constant String := "Sunday";

    end Week;

Если мы хотим создать дочерний пакет для :ada:`Week`, мы можем написать:

.. code-block:: ada
    :class: ada-syntax-only

    package Week.Child is

       function Get_First_Of_Week return String;

    end Week.Child;

Здесь :ada:`Week` ‑ это родительский пакет, а :ada:`Child` ‑ дочерний. Это соответствующее
тело пакета :ada:`Week.Child`:

.. code-block:: ada

    package body Week.Child is

       function Get_First_Of_Week return String is
       begin
          return Mon;
       end Get_First_Of_Week;

    end Week.Child;

В реализации функции :ada:`Get_First_Of_Week` мы можем использовать строку :ada:`Mon` напрямую, даже
если она была объявлена в родительском пакете :ada:`Week`. Мы не пишем здесь :ada:`with Week`,
потому что все элементы из спецификации пакета :ada:`Week`, такие как :ada:`Mon`, :ada:`Tue` и т. д.,
видны в дочернем пакете :ada:`Week.Child`.

Теперь, когда мы завершили реализацию пакета :ada:`Week.Child`, мы можем использовать
элементы из этого дочернего пакета в подпрограмме, просто написав :ada:`with Week.Child`.
Точно так же, если мы хотим использовать эти элементы напрямую, мы
дополнительно пишем :ada:`use Week.Child`. Например:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;
    with Week.Child;  use Week.Child;

    procedure Main is
    begin
       Put_Line ("First day of the week is "
                 & Get_First_Of_Week);
    end Main;

Дочерний пакет от дочернего пакета
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

До сих пор мы видели двухуровневую иерархию пакетов. Но иерархия,
которую мы потенциально можем создать, этим не ограничивается.
Например, мы могли бы расширить иерархию предыдущего примера исходного
кода, объявив пакет :ada:`Week.Child.Grandchild`. В этом случае :ada:`Week.Child` будет родительским для пакета :ada:`Grandchild`.
Рассмотрим эту реализацию:

.. code-block:: ada

    package Week.Child.Grandchild is

       function Get_Second_Of_Week return String;

    end Week.Child.Grandchild;

    package body Week.Child.Grandchild is

       function Get_Second_Of_Week return String is
       begin
          return Tue;
       end Get_Second_Of_Week;

    end Week.Child.Grandchild;

Мы можем использовать этот новый пакет :ada:`Grandchild` в нашем тестовом приложении
так же, как и раньше: мы можем повторно использовать предыдущее
тестовое приложение и адаптировать :ada:`with`, :ada:`use` и вызов функции. Это обновленный
код:

.. code-block:: ada

    with Ada.Text_IO;           use Ada.Text_IO;
    with Week.Child.Grandchild; use Week.Child.Grandchild;

    procedure Main is
    begin
       Put_Line ("Second day of the week is "
                 & Get_Second_Of_Week);
    end Main;

Опять же, это не предел иерархии пакетов. Мы могли бы продолжить
расширение иерархии предыдущего примера, реализовав пакет :ada:`Week.Child.Grandchild.Grand_grandchild`.

Множественные потомки
~~~~~~~~~~~~~~~~~~~~~

До сих пор мы видели один дочерний пакет родительского пакета. Однако
родительский пакет также может иметь несколько дочерних элементов. Мы
могли бы расширить приведенный выше пример и реализовать пакет :ada:`Week.Child_2`.
Например:

.. code-block:: ada

    package Week.Child_2 is

       function Get_Last_Of_Week return String;

    end Week.Child_2;

Здесь :ada:`Week` по-прежнему является родительским пакетом для пакета :ada:`Child`, но это
также родительский пакет для пакета :ada:`Child_2`. Таким же образом, :ada:`Child_2`, очевидно,
является одним из дочерних пакетов :ada:`Week`.

Это соответствующее тело пакета :ada:`Week.Child_2`:

.. code-block:: ada

    package body Week.Child_2 is

       function Get_Last_Of_Week return String is
       begin
          return Sun;
       end Get_Last_Of_Week;

    end Week.Child_2;

Теперь мы можем ссылаться на обоих потомков в нашем тестовом
приложении:

.. code-block:: ada

    with Ada.Text_IO;  use Ada.Text_IO;
    with Week.Child;   use Week.Child;
    with Week.Child_2; use Week.Child_2;

    procedure Main is
    begin
       Put_Line ("First day of the week is "
                 & Get_First_Of_Week);
       Put_Line ("Last day of the week is "
                 & Get_Last_Of_Week);
    end Main;

Видимость
~~~~~~~~~

В предыдущем разделе мы видели, что элементы, объявленные в
спецификации родительского пакета, видны в дочернем пакете. Однако это
не относится к элементам, объявленным в теле родительского пакета.

Рассмотрим пакет :ada:`Book` и его дочерний элемент :ada:`Additional_Operations`:

.. code-block:: ada

    package Book is

       Title : constant String :=
         "Visible for my children";

       function Get_Title return String;

       function Get_Author return String;

    end Book;

    package Book.Additional_Operations is

       function Get_Extended_Title return String;

       function Get_Extended_Author return String;

    end Book.Additional_Operations;

Это тело обоих пакетов:

.. code-block:: ada

    package body Book is

       Author : constant String :=
         "Author not visible for my children";

       function Get_Title return String is
       begin
          return Title;
       end Get_Title;

       function Get_Author return String is
       begin
          return Author;
       end Get_Author;

    end Book;

    package body Book.Additional_Operations is

       function Get_Extended_Title return String is
       begin
          return "Book Title: " & Title;
       end Get_Extended_Title;

       function Get_Extended_Author return String is
       begin
          --  "Author" string declared in the body
          --  of the Book package is not visible
          --  here. Therefore, we cannot write:
          --
          --  return "Book Author: " & Author;

          return "Book Author: Unknown";
       end Get_Extended_Author;

    end Book.Additional_Operations;

В реализации :ada:`Get_Extended_Title` мы используем константу :ada:`Title` из родительского пакета :ada:`Book`.
Однако, как указано в комментариях к функции :ada:`Get_Extended_Author`, строка :ada:`Author`, которую мы
объявили в теле пакета :ada:`Book`, не отображается в пакете :ada:`Book.Additional_Operations`. Следовательно, мы
не можем использовать его для реализации функции :ada:`Get_Extended_Author`.

Однако мы можем использовать функцию :ada:`Get_Author` из :ada:`Book` в реализации функции :ada:`Get_Extended_Author` для
получения этой строки. Точно так же мы можем использовать эту
стратегию для реализации функции :ada:`Get_Extended_Title`. Это адаптированный код:

.. code-block:: ada

    package body Book.Additional_Operations is

       function Get_Extended_Title return String is
       begin
          return "Book Title: " & Get_Title;
       end Get_Extended_Title;

       function Get_Extended_Author return String is
       begin
          return "Book Author: " & Get_Author;
       end Get_Extended_Author;

    end Book.Additional_Operations;

Это простое тестовое приложение для указанных выше пакетов:

.. code-block:: ada

    with Ada.Text_IO;                use Ada.Text_IO;
    with Book.Additional_Operations; use Book.Additional_Operations;

    procedure Main is
    begin
       Put_Line (Get_Extended_Title);
       Put_Line (Get_Extended_Author);
    end Main;

Объявляя элементы в теле пакета, мы можем реализовать инкапсуляцию в
Ada. Эти элементы будут видны только в теле пакета, но нигде больше.
Однако это не единственный способ добиться инкапсуляции в Ada: мы
обсудим другие подходы в главе :doc:`./privacy`.

.. _Package_Renaming:

Переименование
--------------

Ранее мы упоминали, что
:ref:`подпрограммы можно переименовывать <Subprogram_Renaming>`. Мы также
можем переименовывать пакеты. Опять же, для этого мы используем
ключевое слово :ada:`renames`. В следующем примере пакет :ada:`Ada.Text_IO`
переименовывается как :ada:`T_IO`:

.. code-block:: ada

    with Ada.Text_IO;

    procedure Main is
       package TIO renames Ada.Text_IO;
    begin
       TIO.Put_Line ("Hello");
    end Main;

Мы можем использовать переименование, чтобы улучшить читаемость нашего
кода, используя более короткие имена пакетов. В приведенном выше
примере мы пишем :ada:`TIO.Put_Line` вместо более длинной версии
(:ada:`Ada.Text_IO.Put_Line`). Этот подход особенно
полезен, когда мы не вводим спецификатор использования :ada:`use` пакета,
но хотим, чтобы код не становился слишком многословным.

Обратите внимание, что мы также можем переименовывать подпрограммы и
объекты внутри пакетов. Например, мы могли бы просто переименовать
процедуру :ada:`Put_Line` в приведенном выше примере исходного кода:

.. code-block:: ada

    with Ada.Text_IO;

    procedure Main is
       procedure Say (Something : String)
         renames Ada.Text_IO.Put_Line;
    begin
       Say ("Hello");
    end Main;


