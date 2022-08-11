Модульное программирование
==========================

.. include:: ../../global.txt

До сих пор наши примеры были простыми автономными подпрограммами.
Возможность расположить произвольные объявления в зоне описания
способствует этому. Благодоря этой возможности мы смогли объявить
наши типы и переменные в телах основных процедур.

Однако легко увидеть, что такой подход не вписывается в масштаб реальных
приложений. Нам нужен лучший способ структурировать наши программы в
модульные и отдельные блоки.

Ада поощряет разделение программ на несколько пакетов и подпакетов,
предоставляя программисту множество инструментов в поисках идеальной
организации кода.

Пакеты
------

Вот пример объявления пакета в Аде:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Modular_Programming.Week

    package Week is

       Mon : constant String := "Monday";
       Tue : constant String := "Tuesday";
       Wed : constant String := "Wednesday";
       Thu : constant String := "Thursday";
       Fri : constant String := "Friday";
       Sat : constant String := "Saturday";
       Sun : constant String := "Sunday";

    end Week;

А вот как вы его используете:

.. code:: ada run_button project=Courses.Intro_To_Ada.Modular_Programming.Week

    with Ada.Text_IO; use Ada.Text_IO;
    with Week;
    --  References the Week package, and
    --  adds a dependency from Main to Week

    procedure Main is
    begin
       Put_Line ("First day of the week is "
                 & Week.Mon);
    end Main;

Пакеты позволяют вам сделать код модульным, разбивая программы на
семантически значимые единицы. Кроме того, отделение спецификации
пакета от его тела (которое мы увидим ниже) может сократить время
компиляции.

Хотя спецификатор контекста :ada:`with` указывает зависимость,
в приведенном выше примере видно, что нам всё еще нужно использовать
префикс с менем пакета, чтобы сослаться на имя в этом пакете.
(Если бы мы также указали спецификатор использования
:ada:`use Week`, то такой префикс уже не был бы необходим.)

При доступе к объектам из пакета используется точечная нотация :ada:`A.B`,
аналогичная той, что используется для доступа к полям записей.

Спецификатор контекста :ada:`with` может появляться *только* в начале
модуля компиляции (т. е. перед зарезервированным словом, таким как
:ada:`procedure`, которое отмечает начало модуля).
В других местах он запрещен. Это правило необходимо
только по методологическим соображениям: человек, читающий ваш код,
должен сразу видеть, от каких модулей он зависит.

.. admonition:: На других языках

    Пакеты похожи на файлы заголовков в C / C ++, но семантически сильно
    отличаются от них.

     - Первое и самое важное отличие состоит в том, что пакеты представляют
       собой механизм уровня языка. В то время, как заголовочный файл из
       :c:`#include` обрабатывается препроцессором C.

     - Непосредственным следствием этого является то, что конструкция :ada:`with`
       работает на семантическом уровне, а не с помощью подстановки
       текста. Следовательно, когда вы работаете с пакетом указывая :ada:`with`,
       вы говорите компилятору: «Я зависим от этой семантической единицы», а не
       «включите сюда эту кучу текста».

     - Таким образом, действие пакета не зависит от того, откуда на него ссылается
       :ada:`with`. Сравните это с C/C++, где смысл включенного текста может меняться
       в зависимости от контекста, в котором появляется :c:`#include`.

       Это позволяет повысить эффективность компиляции/перекомпиляции. Это
       также облегчает инструментам, таким как IDE, получать правильную
       информацию о семантике программы. Что, в свою очередь, позволяет иметь
       лучший инструментарий в целом и код, который легче поддается анализу даже
       людьми.

    Важным преимуществом :ada:`with` в Аде по сравнению с :c:`#include`
    является то, что он не имеет состояния.
    Порядок спецификаторов :ada:`with` и :ada:`use` не имеет значения и может
    быть изменен без побочных эффектов.

.. admonition:: В наборе инструментов GNAT

    Стандарт языка Ада не предусматривает каких-либо особых отношений
    между исходными файлами и пакетами; например, теоретически вы можете
    поместить весь свой код в один файл или использовать свои собственные
    соглашения об именовании файлов. На практике, однако, каждая реализация
    имеет свои правила. Для GNAT каждый модуль компиляции
    верхнего уровня должен быть помещен в отдельный файл. В приведенном
    выше примере пакет :ada:`Week` будет находиться в файле ``.ads``
    (для Ада спецификации), а основная процедура :ada:`Main` - в файле
    ``.adb`` (для Ада тела).

Использование пакета
--------------------

Как мы видели выше, спецификатор контекста :ada:`with` указывает на зависимость
от другого пакета. Тем не менее, каждая ссылка на сущъность, находящуюся
в пакете :ada:`Week`, должна иметь префикс в виде полного имени пакета.
Можно сделать все сущъности пакета непосредственно видимым в текущей области
с помощью спецификатора использования :ada:`use`.

Фактически, мы использовали спецификатор :ada:`use` почти с самого начала этого
руководства.

.. code:: ada run_button project=Courses.Intro_To_Ada.Modular_Programming.Week

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

-  :ada:`Put_Line` - это подпрограмма из пакета :ada:`Ada.Text_IO`. Мы можем
   ссылаться на нее непосредственно, потому что мы указали пакет в :ada:`use`
   в верхней части основного модуля :ada:`Main`.

-  В отличие от спецификатора контекста :ada:`with`, спецификатор использования
   :ada:`use` может быть помещен либо в заголовок, либо в любую область
   описания. В последнем случае эффект :ada:`use` будет распространяться
   только на соответствующую область видимости.

Тело пакета
-----------

В приведенном выше простом примере пакет :ada:`Week` содержит только объявления и
не содержит реализаций. Это не ошибка: в спецификации пакета,
которая проиллюстрирована выше, нельзя объявлять реализации.
Реализации должны находиться в теле пакета.

.. code:: ada compile_button project=Courses.Intro_To_Ada.Modular_Programming.Operations

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

Здесь мы видим, что тело функции :ada:`Increment_By` должно быть объявлено
в теле. Пользуясь появлением тела можно поместить переменную
:ada:`Last_Increment` в тело, и сделать ее недоступной для пользователя пакета
:ada:`Operations`, обеспечив первую форму инкапсуляции.

Это работает, поскольку объекты, объявленные в теле, видимы *только* в теле.

В этом примере показано, как непосредственно использовать :ada:`Increment_By`:

.. code:: ada run_button project=Courses.Intro_To_Ada.Modular_Programming.Operations

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

    Ада также поддерживает вложенные пакеты. Однако, поскольку их
    использование может быть более сложным, рекомендуется использовать
    дочерние пакеты. Вложенные пакеты будут рассмотрены в расширенном
    курсе.

Давайте начнем обсуждение дочерних пакетов с нашего предыдущего пакета
:ada:`Week`:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Modular_Programming.Child_Packages

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

.. code:: ada no_button project=Courses.Intro_To_Ada.Modular_Programming.Child_Packages
    :class: ada-syntax-only

    package Week.Child is

       function Get_First_Of_Week return String;

    end Week.Child;

Здесь :ada:`Week` - это родительский пакет, а :ada:`Child` - дочерний. Это соответствующее
тело пакета :ada:`Week.Child`:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Modular_Programming.Child_Packages

    package body Week.Child is

       function Get_First_Of_Week return String is
       begin
          return Mon;
       end Get_First_Of_Week;

    end Week.Child;

В реализации функции :ada:`Get_First_Of_Week` мы можем использовать строку
:ada:`Mon` непосредственно, хотя она объявлена в родительском пакете
:ada:`Week`. Мы не пишем здесь :ada:`with Week`, потому что все элементы из
спецификации пакета :ada:`Week`, такие как :ada:`Mon`, :ada:`Tue` и т. д.,
видны в дочернем пакете :ada:`Week.Child`.

Теперь, когда мы завершили реализацию пакета :ada:`Week.Child`, мы можем использовать
элементы из этого дочернего пакета в подпрограмме, просто написав :ada:`with Week.Child`.
Точно так же, если мы хотим использовать эти элементы непосредственно, мы
дополнительно напишем :ada:`use Week.Child`. Например:

.. code:: ada run_button project=Courses.Intro_To_Ada.Modular_Programming.Child_Packages

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
кода, объявив пакет :ada:`Week.Child.Grandchild`. В этом случае
:ada:`Week.Child` будет родительским для пакета :ada:`Grandchild`.
Рассмотрим эту реализацию:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Modular_Programming.Child_Packages

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
тестовое приложение адаптировав :ada:`with`, :ada:`use` и вызов функции.
Вот обновленный код:

.. code:: ada run_button project=Courses.Intro_To_Ada.Modular_Programming.Child_Packages

    with Ada.Text_IO;           use Ada.Text_IO;
    with Week.Child.Grandchild; use Week.Child.Grandchild;

    procedure Main is
    begin
       Put_Line ("Second day of the week is "
                 & Get_Second_Of_Week);
    end Main;

Опять же, это не предел иерархии пакетов. Мы могли бы продолжить
расширение иерархии предыдущего примера, реализовав пакет
:ada:`Week.Child.Grandchild.Grand_grandchild`.

Множественные потомки
~~~~~~~~~~~~~~~~~~~~~

До сих пор мы видели лишь один дочерний пакет родительского пакета. Однако
родительский пакет также может иметь несколько дочерних. Мы
могли бы расширить приведенный выше пример и создать пакет :ada:`Week.Child_2`.
Например:

.. code:: ada no_button project=Courses.Intro_To_Ada.Modular_Programming.Child_Packages
    :class: ada-syntax-only

    package Week.Child_2 is

       function Get_Last_Of_Week return String;

    end Week.Child_2;

Здесь :ada:`Week` по-прежнему является родительским пакетом для пакета :ada:`Child`, но
также родительским пакетом и для пакета :ada:`Child_2`. Таким же образом, :ada:`Child_2`,
очевидно, является одним из дочерних пакетов :ada:`Week`.

Это соответствующее тело пакета :ada:`Week.Child_2`:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Modular_Programming.Child_Packages

    package body Week.Child_2 is

       function Get_Last_Of_Week return String is
       begin
          return Sun;
       end Get_Last_Of_Week;

    end Week.Child_2;

Теперь мы можем сослаться на оба потомка в нашем тестовом приложении:

.. code:: ada run_button project=Courses.Intro_To_Ada.Modular_Programming.Child_Packages

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

.. code:: ada no_button project=Courses.Intro_To_Ada.Modular_Programming.Visibility
    :class: ada-syntax-only

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

Это тела обоих пакетов:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Modular_Programming.Visibility

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

Однако мы можем использовать функцию :ada:`Get_Author` из :ada:`Book`
в реализации функции :ada:`Get_Extended_Author` для
получения этой строки. Точно так же мы можем использовать эту
стратегию для реализации функции :ada:`Get_Extended_Title`. Это адаптированный код:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Modular_Programming.Visibility

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

Вот простое тестовое приложение для указанных выше пакетов:

.. code:: ada run_button project=Courses.Intro_To_Ada.Modular_Programming.Visibility

    with Ada.Text_IO;                use Ada.Text_IO;
    with Book.Additional_Operations; use Book.Additional_Operations;

    procedure Main is
    begin
       Put_Line (Get_Extended_Title);
       Put_Line (Get_Extended_Author);
    end Main;

Объявляя элементы в теле пакета, мы можем реализовать инкапсуляцию в языке
Ада. Эти элементы будут видимы только в теле пакета, но нигде больше.
Но это не единственный способ добиться инкапсуляции в Аде: мы
обсудим другие подходы в главе :doc:`./privacy`.

.. _Package_Renaming:

Переименование
--------------

Ранее мы упоминали, что
:ref:`подпрограммы можно переименовывать <Subprogram_Renaming>`. Мы также
можем переименовывать пакеты. Опять же, для этого мы используем
ключевое слово :ada:`renames`. В следующем примере пакет :ada:`Ada.Text_IO`
переименовывается как :ada:`TIO`:

.. code:: ada run_button project=Courses.Intro_To_Ada.Modular_Programming.Rename_Text_IO

    with Ada.Text_IO;

    procedure Main is
       package TIO renames Ada.Text_IO;
    begin
       TIO.Put_Line ("Hello");
    end Main;

Мы можем использовать переименование, чтобы улучшить читаемость нашего
кода, используя более короткие имена пакетов. В приведенном выше
примере мы пишем :ada:`TIO.Put_Line` вместо более длинного имени
(:ada:`Ada.Text_IO.Put_Line`). Этот подход особенно
полезен, когда мы не используем спецификатор использования :ada:`use`,
но хотим, чтобы код не становился слишком многословным.

Обратите внимание, что мы также можем переименовывать подпрограммы и
объекты внутри пакетов. Например, мы могли бы просто переименовать
процедуру :ada:`Put_Line` в приведенном выше примере исходного кода:

.. code:: ada run_button project=Courses.Intro_To_Ada.Modular_Programming.Rename_Put_Line

    with Ada.Text_IO;

    procedure Main is
       procedure Say (Something : String)
         renames Ada.Text_IO.Put_Line;
    begin
       Say ("Hello");
    end Main;


