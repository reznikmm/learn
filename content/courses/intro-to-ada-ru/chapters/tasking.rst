Управление задачами
===================

.. include:: ../../global.txt

Задачи и защищенные объекты позволяют реализовать параллелизм в Ada. В
следующих разделах эти концепции объясняются более подробно.

Задачи
------

Задачу можно рассматривать как приложение, которое выполняется
одновременно (*concurrently*) с основным приложением. В других языках
программирования задачу можно назвать
`потоком <https://en.wikipedia.org/wiki/Thread_(computing)>`_,
а постановку задач можно назвать
`многопоточностью
<https://en.wikipedia.org/wiki/Thread_(computing)#Multithreading>`_.

Задачи могут синхронизироваться с основным приложением, но также могут
обрабатывать информацию полностью независимо от основного приложения.
Здесь мы покажем, как это достигается.

Простая задача
~~~~~~~~~~~~~~

Задачи объявляются с использованием ключевого слова :ada:`task`. Реализация
задачи определяется в блоке тела задачи (:ada:`task body`). Например:

.. code:: ada run_button project=Courses.Intro_To_Ada.Tasking.Show_Simple_Task

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Simple_Task is
       task T;

       task body T is
       begin
          Put_Line ("In task T");
       end T;
    begin
       Put_Line ("In main");
    end Show_Simple_Task;

Здесь мы объявляем и реализуем задачу :ada:`T`. Как только запускается
основное приложение, задача :ada:`T` запускается автоматически - не
обязательно вручную запускать эту задачу. Запустив приложение выше, мы
видим, что выполняются оба вызова :ada:`Put_Line`.

Обратите внимание, что:

-  Основное приложение само по себе является задачей (основной задачей).

   -  В этом примере подпрограмма :ada:`Show_Simple_Task` является основной
      задачей приложения.

-  Задача :ada:`T` - это подзадача.

   -  Каждая подзадача имеет главную задачу.

   -  Поэтому основная задача - это также главная задача задачи :ada:`T`.

-  Количество задач не ограничено одной: мы могли бы включить задачу
   :ada:`T2` в приведенный выше пример.

   -  Эта задача также запускается автоматически и выполняется *одновременно*
      как с задачей :ada:`T`, так и с основной задачей. Например:

    .. code:: ada run_button project=Courses.Intro_To_Ada.Tasking.Multiple_Simple_Task

          with Ada.Text_IO; use Ada.Text_IO;

                procedure Show_Simple_Tasks is
                   task T;
                   task T2;

                   task body T is
                   begin
                      Put_Line ("In task T");
                   end T;

                   task body T2 is
                   begin
                      Put_Line ("In task T2");
                   end T2;

                begin
                   Put_Line ("In main");
                end Show_Simple_Tasks;

Простая синхронизация
~~~~~~~~~~~~~~~~~~~~~

Как мы только что видели, как только запускается основная задача,
автоматически запускаются и ее подзадачи. Основная задача продолжает
свою обработку до тех пор, пока ей больше нечего делать. Однако на
этом этапе он не завершится. Вместо этого задача ожидает завершения
выполнения своих подзадач, прежде чем позволить себе завершиться.
Другими словами, этот процесс ожидания обеспечивает синхронизацию
между основной задачей и ее подзадачами. После этой синхронизации
основная задача завершится. Например:

.. code:: ada run_button project=Courses.Intro_To_Ada.Tasking.Show_Simple_Sync

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Simple_Sync is
       task T;
       task body T is
       begin
          for I in 1 .. 10 loop
             Put_Line ("hello");
          end loop;
       end T;
    begin
       null;
       --  Will wait here until all tasks
       --  have terminated
    end Show_Simple_Sync;

Тот же механизм используется для других подпрограмм, содержащих
подзадачи: главная задача подпрограммы будет ждать завершения своих
подзадач. Таким образом, этот механизм не ограничивается основным
приложением, а также применяется к любой подпрограмме, вызываемой
основным приложением или его подпрограммами.

Синхронизация также происходит, если мы перемещаем задачу в отдельный
пакет. В приведенном ниже примере мы объявляем задачу :ada:`T` в пакете
:ada:`Simple_Sync_Pkg`.

.. code:: ada no_button project=Courses.Intro_To_Ada.Tasking.Simple_Sync_Pkg
    :class: ada-syntax-only

    package Simple_Sync_Pkg is
       task T;
    end Simple_Sync_Pkg;

Это соответствующее тело пакета:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Tasking.Simple_Sync_Pkg

    with Ada.Text_IO; use Ada.Text_IO;

    package body Simple_Sync_Pkg is
       task body T is
       begin
          for I in 1 .. 10 loop
             Put_Line ("hello");
          end loop;
       end T;
    end Simple_Sync_Pkg;

Поскольку пакет обрабатывается (:ada:`with`) основной процедурой, задача
:ada:`T`, определенная в пакете, является частью основной задачи. Например:

.. code:: ada run_button project=Courses.Intro_To_Ada.Tasking.Simple_Sync_Pkg

    with Simple_Sync_Pkg;

    procedure Test_Simple_Sync_Pkg is
    begin
       null;
       --  Will wait here until all tasks
       --  have terminated
    end Test_Simple_Sync_Pkg;

Опять же, как только основная задача достигает своего завершения, она
синхронизируется с задачей :ada:`T` из :ada:`Simple_Sync_Pkg` перед
завершением.

Задержка (delay)
~~~~~~~~~~~~~~~~

Мы можем ввести задержку, используя ключевое слово :ada:`delay`. Это переводит
задачу в спящий режим на время (в секундах), указанное в инструкции
delay. Например:

.. code:: ada run_button project=Courses.Intro_To_Ada.Tasking.Show_Delay

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Delay is

       task T;

       task body T is
       begin
          for I in 1 .. 5 loop
             Put_Line ("hello from task T");
             delay 1.0;
             --    ^ Wait 1.0 seconds
          end loop;
       end T;
    begin
       delay 1.5;
       Put_Line ("hello from main");
    end Show_Delay;

В этом примере мы заставляем задачу :ada:`T` ждать одну секунду после каждого
отображения сообщения "hello". Кроме того, основная задача ожидает 1,5
секунды перед отображением собственного сообщения "hello".

Синхронизация: рандеву
~~~~~~~~~~~~~~~~~~~~~~

Единственный тип синхронизации, который мы видели до сих пор, - это
тот, который происходит автоматически в конце основной задачи. Вы
также можете определить пользовательские точки синхронизации,
используя ключевое слово :ada:`entry`. Запись *entry* можно рассматривать
как особый вид подпрограммы, которая вызывается главной задачей с
использованием аналогичного синтаксиса, как мы увидим позже.

В определении задачи вы определяете, какая часть задачи будет
принимать записи, используя ключевое слово :ada:`accept`. Задача выполняется
до тех пор, пока не достигнет инструкции :ada:`accept`, а затем ожидает
синхронизации с основной задачей. Конкретно:

-  подзадача ожидает в этот момент (в инструкции :ada:`accept`), готовая
   принять вызов соответствующей записи из главной задачи;

-  главная задача вызывает запись задачи способом, аналогичным вызову
   процедуры, для синхронизации с подзадачей.

Эта синхронизация между задачами называется *рандеву*. Давайте
посмотрим на пример:

.. code:: ada run_button project=Courses.Intro_To_Ada.Tasking.Show_Rendezvous

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Rendezvous is

       task T is
          entry Start;
       end T;

       task body T is
       begin
          accept Start;
          --     ^ Waiting for somebody
          --       to call the entry

          Put_Line ("In T");
       end T;

    begin
       Put_Line ("In Main");

       --  Calling T's entry:
       T.Start;
    end Show_Rendezvous;

В этом примере мы объявляем вход :ada:`Start` задачи :ada:`T`. В теле задачи
мы реализуем эту запись с помощью :ada:`accept Start`. Когда задача :ada:`T`
достигает этой точки, она ожидает главную задачу. Эта синхронизация
происходит в инструкции :ada:`T.Start`. После
завершения синхронизации основная задача и задача :ada:`T` снова выполняются
одновременно, пока они не синхронизируются в последний раз, когда
основная задача завершается.

Запись может использоваться для выполнения большего, чем простая
синхронизация задач: она также может выполнять несколько инструкций в
течение времени синхронизации обеих задач. Мы делаем это с помощью
блока :ada:`do ... end`. Для предыдущего примера мы бы просто написали
:ada:`accept Start do <операторы>; end;`. Мы используем этот тип блока
в следующем примере.

Цикл выбора (Select)
~~~~~~~~~~~~~~~~~~~~

Количество раз, когда заявка может быть принята, не ограничено. Мы
могли бы даже создать бесконечный цикл в задаче и принимать вызовы к
одной и той же записи снова и снова. Однако бесконечный цикл
предотвращает завершение подзадачи, поэтому он блокирует основную
задачу, когда она достигает конца своей обработки. Поэтому цикл,
содержащий инструкции :ada:`accept` в теле задачи, обычно используется в
сочетании с инструкцией :ada:`select ... or terminate` (выбрать или заявить
о прекращении действия). Проще говоря, этот оператор позволяет главной задаче
автоматически завершать выполнение подзадачи по завершении основной задачи.
Например:

.. code:: ada run_button project=Courses.Intro_To_Ada.Tasking.Show_Rendezvous_Loop

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Rendezvous_Loop is

       task T is
          entry Reset;
          entry Increment;
       end T;

       task body T is
          Cnt : Integer := 0;
       begin
          loop
             select
                accept Reset do
                   Cnt := 0;
                end Reset;
                Put_Line ("Reset");
             or
                accept Increment do
                   Cnt := Cnt + 1;
                end Increment;
                Put_Line ("In T's loop ("
                          & Integer'Image (Cnt)
                          & ")");
             or
                terminate;
             end select;
          end loop;
       end T;

    begin
       Put_Line ("In Main");

       for I in 1 .. 4 loop
          --  Calling T's entry multiple times
          T.Increment;
       end loop;

       T.Reset;
       for I in 1 .. 4 loop
          --  Calling T's entry multiple times
          T.Increment;
       end loop;

    end Show_Rendezvous_Loop;

В этом примере тело задачи реализует бесконечный цикл, который
принимает вызовы записей :ada:`Reset` и :ada:`Increment`. Делаем следующие
наблюдения:

-  Блок :ada:`accept E do ... end` используется для увеличения счетчика.

   -  До тех пор, пока задача :ada:`T` выполняет блок :ada:`do ... end`,
      основная задача ожидает завершения блока.

-  Основная задача - вызов записи :ada:`Increment` несколько раз в цикле
   от :ada:`1 .. 4`. Она также вызывает запись :ada:`Reset` перед циклом.

   -  Поскольку задача :ada:`T` содержит бесконечный цикл, она всегда
      принимает вызовы записей :ada:`Reset` и :ada:`Increment`.

   -  Когда основная задача завершается, она проверяет статус задачи
      :ada:`T`. Несмотря на то, что задача :ada:`T` может принимать
      новые вызовы записей :ada:`Reset` или :ada:`Increment`, главная
      задача может завершить задачу :ada:`T` введу отсутствия от нее
      вызовов и наличия :ada:`or terminate` части инструкции :ada:`select`.

Циклические задачи
~~~~~~~~~~~~~~~~~~

В предыдущем примере мы видели, как отложить задачу на указанное время
с помощью ключевого слова :ada:`delay`. Однако использования операторов
задержки в цикле недостаточно, чтобы гарантировать регулярные интервалы
между этими операторами задержки. Например, у нас может быть вызов
вычислительно интенсивной процедуры между выполнением последовательных
инструкций задержки:

.. code-block:: ada

    while True loop
       delay 1.0;
       --    ^ Wait 1.0 seconds
       Computational_Intensive_App;
    end loop;

В этом случае мы не можем гарантировать, что после 10 вызовов
инструкции delay прошло ровно 10 секунд, поскольку процедура
:ada:`Computational_Intensive_App` может
привести к смещению времени. Во многих случаях этот временной дрейф не
имеет значения, поэтому достаточно использовать ключевое слово :ada:`delay`.

Однако бывают ситуации, когда временной дрейф недопустим. В этих
случаях нам нужно использовать оператор :ada:`delay until`, который принимает
точное время окончания задержки, позволяя нам определить регулярный интервал.
Это полезно, например, в приложениях реального времени.

Вскоре мы увидим пример того, как может быть введен этот временной
дрейф и как оператор :ada:`delay until` позволяет обойти проблему.
Но прежде чем мы это сделаем, мы рассмотрим пакет, содержащий процедуру,
позволяющую нам измерять прошедшее время (:ada:`Show_Elapsed_Time`) и
фиктивную процедуру :ada:`Computational_Intensive_App`, которая
моделируется с помощью простой задержки. Это полный пакет:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Tasking.Show_Time

    with Ada.Real_Time; use Ada.Real_Time;

    package Delay_Aux_Pkg is

       function Get_Start_Time return Time
         with Inline;

       procedure Show_Elapsed_Time
         with Inline;

       procedure Computational_Intensive_App;
    private
       Start_Time   : Time := Clock;

       function Get_Start_Time return Time is (Start_Time);

    end Delay_Aux_Pkg;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Delay_Aux_Pkg is

       procedure Show_Elapsed_Time is
          Now_Time     : Time;
          Elapsed_Time : Time_Span;
       begin
          Now_Time     := Clock;
          Elapsed_Time := Now_Time - Start_Time;
          Put_Line ("Elapsed time "
                    & Duration'Image (To_Duration (Elapsed_Time))
                    & " seconds");
       end Show_Elapsed_Time;

       procedure Computational_Intensive_App is
       begin
          delay 0.5;
       end Computational_Intensive_App;

    end Delay_Aux_Pkg;

Используя этот вспомогательный пакет, теперь мы готовы написать наше
приложение с дрейфом во времени:

.. code:: ada run_button project=Courses.Intro_To_Ada.Tasking.Show_Time

    with Ada.Text_IO;   use Ada.Text_IO;
    with Ada.Real_Time; use Ada.Real_Time;

    with Delay_Aux_Pkg;

    procedure Show_Time_Task is
       package Aux renames Delay_Aux_Pkg;

       task T;

       task body T is
          Cnt   : Integer := 1;
       begin
          for I in 1 .. 5 loop
             delay 1.0;

             Aux.Show_Elapsed_Time;
             Aux.Computational_Intensive_App;

             Put_Line ("Cycle # "
                       & Integer'Image (Cnt));
             Cnt  := Cnt + 1;
          end loop;
          Put_Line ("Finished time-drifting loop");
       end T;

    begin
       null;
    end Show_Time_Task;

Запустив приложение, мы видим, что у нас уже есть разница во времени
примерно в четыре секунды после трех итераций цикла из-за дрейфа,
вызванного :ada:`Computational_Intensive_App`lications. Однако, используя
оператор :ada:`delay until`, мы можем избежать
этого временного дрейфа и иметь регулярный интервал ровно в одну
секунду:

.. code:: ada run_button project=Courses.Intro_To_Ada.Tasking.Show_Time

    with Ada.Text_IO;   use Ada.Text_IO;
    with Ada.Real_Time; use Ada.Real_Time;

    with Delay_Aux_Pkg;

    procedure Show_Time_Task is
       package Aux renames Delay_Aux_Pkg;

       task T;

       task body T is
          Cycle : constant Time_Span :=
            Milliseconds (1000);
          Next  : Time := Aux.Get_Start_Time
                          + Cycle;

          Cnt   : Integer := 1;
       begin
          for I in 1 .. 5 loop
             delay until Next;

             Aux.Show_Elapsed_Time;
             Aux.Computational_Intensive_App;

             --  Calculate next execution time
             --  using a cycle of one second
             Next := Next + Cycle;

             Put_Line ("Cycle # "
                       & Integer'Image (Cnt));
             Cnt  := Cnt + 1;
          end loop;
          Put_Line ("Finished cycling");
       end T;

    begin
       null;
    end Show_Time_Task;

Теперь, как мы можем видеть, запустив приложение, оператор :ada:`delay until`
гарантирует, что :ada:`Computational_Intensive_App` не нарушает регулярный
интервал в одну секунду между итерациями.

Защищенные объекты
------------------

Когда несколько задач получают доступ к общим данным, может произойти
повреждение этих данных. Например, данные могут быть несогласованными,
если одна задача перезаписывает части информации, которые одновременно
считываются другой задачей. Чтобы избежать подобных проблем и
обеспечить скоординированный доступ к информации, мы используем
*защищенные объекты*.

Защищенные объекты инкапсулируют данные и обеспечивают доступ к этим
данным с помощью *защищенных операций*, которые могут быть
подпрограммами или защищенными записями. Использование защищенных
объектов гарантирует, что данные не будут повреждены из-за условий
«гонки» или другого одновременного доступа.

.. admonition:: Важное замечание.

    Защищенные объекты могут быть реализованы с помощью задач Ada.
    Фактически это был *единственный* возможный способ их реализации в Ада
    83 (первый вариант стандарта языка Ada). Однако использование
    защищённых объектов гораздо проще, чем использование аналогичных
    механизмов, реализованных с использованием только задач. Поэтому
    предпочтительно использовать защищенные объекты, когда ваша основная
    цель - только защита данных.

Простой объект
~~~~~~~~~~~~~~

Вы объявляете защищенный объект с помощью ключевого слова :ada:`protected`.
Синтаксис аналогичен тому, который используется для пакетов: вы можете
объявлять операции (например, процедуры и функции) в общедоступной части, а
данные - в закрытой части. Соответствующая реализация операций
включена в защищенное тело (:ada:`protected body`) объекта. Например:

.. code:: ada run_button project=Courses.Intro_To_Ada.Tasking.Show_Protected_Objects

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Protected_Objects is

       protected Obj is
          --  Operations go here (only subprograms)
          procedure Set (V : Integer);
          function Get return Integer;
       private
          --  Data goes here
          Local : Integer := 0;
       end Obj;

       protected body Obj is
          --  procedures can modify the data
          procedure Set (V : Integer) is
          begin
             Local := V;
          end Set;

          --  functions cannot modify the data
          function Get return Integer is
          begin
             return Local;
          end Get;
       end Obj;

    begin
       Obj.Set (5);
       Put_Line ("Number is: "
                 & Integer'Image (Obj.Get));
    end Show_Protected_Objects;

В этом примере мы определяем две операции для :ada:`Obj`: :ada:`Set` и
:ada:`Get`. Реализация этих операций находится в теле объекта :ada:`Obj`.
Синтаксис, используемый для записи этих операций, такой же, как и для
обычных процедур и функций. Реализация защищенных объектов проста - мы
просто получаем доступ и обновляем локальные данные :ada:`Local` в этих
подпрограммах. Для вызова этих операций в основном приложении мы используем
обозначения с префиксом, например, :ada:`Obj.Get`.

Записи (точки входа)
~~~~~~~~~~~~~~~~~~~~

В дополнение к защищенным процедурам и функциям вы также можете
определить защищенные точки входа. Сделайте это, используя ключевое
слово :ada:`entry`. Защищенные точки входа позволяют определять барьеры с
помощью ключевого слова :ada:`when`. Барьеры - это условия, которые должны
быть выполнены до того, как запись сможет начать выполнять свою фактическую
обработку - мы говорим о *снятии* барьера, когда условие выполнено.

В предыдущем примере использовались процедуры и функции для
определения операций с защищенными объектами. Однако это позволяет
считывать защищенную информацию (через :ada:`Obj.Get`) до того, как он будет
установлен (через :ada:`Obj.Set`). Чтобы это было определенной операцией, мы
указали значение по умолчанию (0). Вместо этого, переписав :ada:`Obj.Get`
используя запись (*entry*) вместо функции, мы реализуем барьер, гарантирующий,
что ни одна задача не сможет прочитать информацию до того, как она будет
установлена.

В следующем примере реализован барьер для операции :ada:`Obj.Get`. Он также
содержит две параллельные подпрограммы (основная задача и задача :ada:`T`),
которые пытаются получить доступ к защищаемому объекту.

.. code:: ada run_button project=Courses.Intro_To_Ada.Tasking.Show_Protected_Objects_Entries

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Protected_Objects_Entries is

       protected Obj is
          procedure Set (V : Integer);
          entry Get (V : out Integer);
       private
          Local  : Integer;
          Is_Set : Boolean := False;
       end Obj;

       protected body Obj is
          procedure Set (V : Integer) is
          begin
             Local := V;
             Is_Set := True;
          end Set;

          entry Get (V : out Integer)
            when Is_Set is
             --  Entry is blocked until the
             --  condition is true. The barrier
             --  is evaluated at call of entries
             --  and at exits of procedures and
             --  entries. The calling task sleeps
             --  until the barrier is released.
          begin
             V := Local;
             Is_Set := False;
          end Get;
       end Obj;

       N : Integer := 0;

       task T;

       task body T is
       begin
          Put_Line ("Task T will delay for 4 seconds...");
          delay 4.0;
          Put_Line ("Task T will set Obj...");
          Obj.Set (5);
          Put_Line ("Task T has just set Obj...");
       end T;
    begin
       Put_Line ("Main application will get Obj...");
       Obj.Get (N);
       Put_Line ("Main application has just retrieved Obj...");
       Put_Line ("Number is: " & Integer'Image (N));

    end Show_Protected_Objects_Entries;

Как видим, запустив его, основное приложение ждет, пока не будет
установлен защищенный объект (по вызову :ada:`Obj.Set` в задаче :ada:`T`),
прежде чем прочитает информацию (через :ada:`Obj.Get`). Поскольку в задаче
:ada:`T` добавлена 4-секундная задержка, основное приложение также
задерживается на 4 секунды. Только после этой задержки задача :ada:`T`
устанавливает объект и освобождает барьер в :ada:`Obj.Get`, чтобы главное
приложение могло затем возобновить обработку (после извлечения информации
из защищенного объекта).

Задачи и защищенные типы
------------------------

В предыдущих примерах мы определили единичные задачи и защищенные
объекты. Однако мы можем обобщать задачи и защищенные объекты,
используя определения типов. Это позволяет нам, например, создавать
несколько задач на основе только одного типа задач.

.. _TaskTypes:

Типы задач
~~~~~~~~~~

Тип задачи - это обобщение задачи. Объявление аналогично простым
задачам: вы заменяете :ada:`task` на :ada:`task type`. Разница между простыми
задачами и типами задач заключается в том, что типы задач не создают
фактические задачи, которые запускаются автоматически.
Вместо этого требуется объявление
задачи. Именно так работают обычные переменные и типы: объекты
создаются только с помощью определений переменных, а не определений
типов.

Чтобы проиллюстрировать это, мы повторим наш первый пример:

.. code:: ada run_button project=Courses.Intro_To_Ada.Tasking.Show_Simple_Task

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Simple_Task is
       task T;

       task body T is
       begin
          Put_Line ("In task T");
       end T;
    begin
       Put_Line ("In main");
    end Show_Simple_Task;

Теперь мы переписываем его, заменив задачу :ada:`task T` типом задачи
:ada:`task type TT`. Объявляем задачу (:ada:`A_Task`) на основе типа
задачи :ada:`TT` после её определения:

.. code:: ada run_button project=Courses.Intro_To_Ada.Tasking.Show_Simple_Task_Type

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Simple_Task_Type is
       task type TT;

       task body TT is
       begin
          Put_Line ("In task type TT");
       end TT;

       A_Task : TT;
    begin
       Put_Line ("In main");
    end Show_Simple_Task_Type;

Мы можем расширить этот пример и создать массив задач. Поскольку мы
используем тот же синтаксис, что и для объявлений переменных, мы
используем аналогичный синтаксис для типов задач:
:ada:`array (<>) of Task_Type`. Кроме того, мы
можем передавать информацию отдельным задачам, определяя начальную
запись :ada:`Start`. Вот обновленный пример:

.. code:: ada run_button project=Courses.Intro_To_Ada.Tasking.Show_Task_Type_Array

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Task_Type_Array is
       task type TT is
          entry Start (N : Integer);
       end TT;

       task body TT is
          Task_N : Integer;
       begin
          accept Start (N : Integer) do
             Task_N := N;
          end Start;
          Put_Line ("In task T: "
                    & Integer'Image (Task_N));
       end TT;

       My_Tasks : array (1 .. 5) of TT;
    begin
       Put_Line ("In main");

       for I in My_Tasks'Range loop
          My_Tasks (I).Start (I);
       end loop;
    end Show_Task_Type_Array;

В этом примере мы объявляем пять задач в массиве :ada:`My_Tasks`.
Мы передаем индекс массива отдельным задачам в точке входа (:ada:`Start`).
После синхронизации между отдельными подзадачами и основной задачей каждая
подзадача одновременно вызывает :ada:`Put_Line`.

Защищенные типы
~~~~~~~~~~~~~~~

Защищенный тип - это обобщение защищенного объекта. Объявление
аналогично объявлению для защищенных объектов: вы заменяете :ada:`protected`
на :ada:`protected type`. Как
и типы задач, защищенные типы требуют объявления объекта для создания
реальных объектов. Опять же, это похоже на объявления переменных и
позволяет создавать массивы (или другие составные объекты) защищенных
объектов.

Мы можем повторно использовать предыдущий пример и переписать его,
чтобы использовать защищенный тип:

.. code:: ada run_button project=Courses.Intro_To_Ada.Tasking.Show_Protected_Object_Type

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Protected_Object_Type is

       protected type Obj_Type is
          procedure Set (V : Integer);
          function Get return Integer;
       private
          Local : Integer := 0;
       end Obj_Type;

       protected body Obj_Type is
          procedure Set (V : Integer) is
          begin
             Local := V;
          end Set;

          function Get return Integer is
          begin
             return Local;
          end Get;
       end Obj_Type;

       Obj : Obj_Type;
    begin
       Obj.Set (5);
       Put_Line ("Number is: "
                 & Integer'Image (Obj.Get));
    end Show_Protected_Object_Type;

В этом примере вместо непосредственного определения объекта :ada:`Obj` мы
сначала определяем защищенный тип :ada:`Obj_Type` а затем объявляем
:ada:`Obj` как объект этого защищенного типа. Обратите внимание, что
основное приложение не изменилось: мы по-прежнему используем :ada:`Obj.Set`
и :ada:`Obj.Get` для доступа к защищенному объекту, как в исходном примере.
