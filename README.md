Если вы работаете с одной базой данных которая поддерживает транзакции вы даже не задумываетесь о консистентности - база все делает за вас. Если же у вас несколько баз, распределенная система или даже к примеру MongoDB до 4 версии - все не так радужно. 

Рассмотрим пример - мы хотим сохранить файл в хранилище и добавить ссылку на него в два документа. Конечно же мы хотим атомарности - либо файл сохранен и добавлен в документы либо ни то ни другое (тут и далее используется cats-effects IO):
```scala
saveDataToFile(data) // (1)
  .flatMap { file =>
    addFileRef(documentId, file) // (2)
      .flatMap { result =>
        addFileRef(fileRegistry, file) // (3)
          .flatMap { result =>
            ??? // (4, 5, ...)
          }
          .handleErrorWith { error =>
            // revert (2)
            removeFileRef(documentId, file).attempt >> IO.raiseError(error)
          }
      }
      .handleErrorWith { error =>
        // revert (1)
        removeFile(file).attempt >> IO.raiseError(error)
      }
  }
```
Уже непросто? Легко представить как количество операций растет и образуется Pyramid of doom. 

Но мы же программисты! Давайте обобщим проблему и напишем код, который позволит избежать ненужной сложности и возможных ошибок.

<cut />

### Описываем проблему
Итак, у нас есть ряд действий, которые могут закончиться неудачей. В случае ошибки на любом шаге мы хотим отменить (или компенсировать) успешные действия чтобы гарантировать "атомарность" всей операции. 

<spoiler title="Немного про паттерн Сага и ограничения">
По-сути эта проблема решается использованием паттерна [Saga](https://blog.couchbase.com/saga-pattern-implement-business-transactions-using-microservices-part/), который состоит в объединении действий и компенсаций для последующего трекинга какие действия были выполнены и какие компенсации соответственно нужны для отката. В основном этот паттерн используется в контексте микросервисов и часто имплементируется с использованием очередей сообщений и/или стейт машин, но это не обязательно. В этой статье я рассматриваю решение проблемы на уровне одного процесса, что может быть недостаточно для многих ситуаций в зависимости от бизнес требований. Я не рассматриваю случаи падения процесса целиком в середине транзакции или ошибки при выполнении компенсации - цель статьи показать как можно обобщить и имплементировать на Scala решение подобной проблемы.
</spoiler>

### Пишем первый код
Собственно в правильной постановке задачи уже содержится часть решения - опишем все части в виде одной структуры: 
- действие
- компенсацию 
- дополнительно добавим коммит - закрепление результата когда все шаги транзакции успешно завершены

```scala
final case class Action(
  perform:    ???,
  commit:     ???,
  compensate: ???
)
```

Каким типом описать произвольное действие? Мы можем использовать что-то вроде `() => Try[T]`, но гораздо удобнее использовать `IO` - абстракцию над произвольным действием, я буду использовать имплементацию от [cats](https://typelevel.org/cats-effect/datatypes/io.html).

```scala
final case class Action(
  perform:    IO[Unit],
  commit:     IO[Unit],
  compensate: IO[Unit]
)
```

Глядя на такое объявление сложно сказать будет ли `commit` выполнен до или после `perform`, будет ли `compensate` выполнен если `perform` упадет? 

Для описания подобных отношений отлично подходят функции:

```scala
final case class Action[T](
  perform:    IO[T],
  commit:     T => IO[Unit],
  compensate: T => IO[Unit]
)
```
Теперь очевидно, что выполнить эти действия можно только уже имея `T` - т.е. после успешного выполнения `perform`.

Возвращаясь к нашей изначальной проблеме мы можем написать следующий метод:
```scala
def saveDataToFile(data: Data): IO[File] = ???
def removeFile(file: File): IO[Unit] = ???

def saveDataAction(data: Data): Action[File] = Action(
  perform = saveDataToFile(data),
  compensate = removeFile
)
```

### Шаг второй - композиция
Мы описали одно действие, один шаг транзакции, но как описать несколько шагов? Первым что приходит на ум будет `Seq[Action[_]]` - буквально последовательность шагов. Работать с такой структурой тоже будет довольно легко - мы можем идти по  списку выполняя шаги, а в случае ошибки будем знать какие действия требуют  компенсации.

Но тут есть одна маленькая проблема - что если какой-то из шагов требует  информации о результате предыдущего? Как в нашем примере для добавления в документ ссылки на файл нужно знать путь к файлу после сохранения.

Для того чтобы описать такую композицию введем еще одну структуру специально для композиции:
```scala
final case class ActionChain[A, B](
  first: Action[A],
  next:  A => Action[B]
)
```
эта структура описывает композицию двух действий, что в свою очередь тоже  является действием -- составным. Поэтому объявим общий тип для этих двух структур и получим композицию любого количества действий рекурсивно:
```scala
sealed trait Transaction[T]

final case class Action[T](...) extends Transaction[T]
final case class ActionChain[A, B](
  first: Transaction[A],
  next:  A => Transaction[B]
) extends Transaction[B]
```

Ура! Мы получили описание любой транзакции в виде одного шага или  последовательности шагов, например наша изначальная задача будет выглядеть примерно так:

```scala
ActionChain(
  saveDataAction(data),
  { file => 
    ActionChain(
      addFileRefAction(documentId, file),
      { _ => 
        addFileRefAction(fileRegistry, file)
      }
    )
  }
)
```

Не идеально, но хотя бы компенсации идут вместе с действиями и их больше не забудешь указать. 

Прекрасно, но нехватает маленькой детали - это только описание транзакции, как ее выполнить?

### Выполняем транзакцию
Так как транзакция сама по себе не выполняет никаких действий, а только описывает  что нужно сделать, то для выполнения транзакции нужно собрать ее части в нужном порядке в одну `IO` операцию.

На каждом шаге транзакции у нас нет достаточно информации о том, успешна ли вся транзакция целиком. Поэтому для сборки необходимо "продолжение" - остаток действий до конца транзакции.

Так как наша `Transaction` состоит из звух `case`'ов нам нужно рассмотреть два случая. Если мы хотим выполнить атомарный шаг, то логика следующая:
1. выполнить действие
2. если позникла ошибка ничего делать не надо - действие не удалось и не требует
компенсации
3. если действие удалось нужно продолжать транзакцию до конца
4. по окончанию транзакции нужно подтвердить дейтсвие или же компенсировать его

```scala
private def compile[R](restOfTransaction: T => IO[R]): IO[R] = this match {
  case Action(perform, commit, compensate) => perform.flatMap { t =>
    restOfTransaction(t).redeemWith(
      bind = commit(t).attempt >> IO.pure(_),
      recover = compensate(t).attempt >> IO.raiseError(_)
    )
  }

  case ActionChain(first, next) => ???
}
```

<spoiler title="Для тех, кто не знаком с Cats">
Тут `redeemWith` позволяет выполнить разный код в случае успеха и в случае ошибки,  `attempt` проигнорирует ошибку подтверждения/компенсации (мы же не хотим, чтобы  ошибка компенсации помешала выполнению), `>>` означает "выполнить по очереди", `IO.pure` и `IO.raiseError` вернут оригинальный результат `continue(t)` - значение или ошибку.
</spoiler>

Для выполнения составного действия все еще проще - выполняем одно действие, а потом второе:

```scala
private def compile[R](restOfTransaction: T => IO[R]): IO[R] = this match {
  case Action(perform, commit, compensate) => ...
  
  case ActionChain(first, next) =>
    first.compile { a =>
      next(a).compile { t =>
        restOfTransaction(t)
      }
    }
}
```

Для сборки транзакции целиком нужно просто указать, что больше ничего не осталось:

```scala
sealed trait Transaction[T] {

  def compile: IO[T] = compile(IO.pure) // "продолжение" -- просто вернуть результат
}
```

Теперь после сборки транзакции вместе мы компилируем ее в `IO` и получаем  действие, которое обязательно вызовет `commit`/`compensate` для всех операций, которые успешно выполнились (кроме случаев отмены/прерывания операции в `IO`, но мы не будем тут рассматривать эту возможность).

<spoiler title="Доказательство">

Возможно вы не поверите мне на слово когда я утверждаю, что это решение работает верно. Вы полагаете, что я написал много тестов для проверки? Вовсе нет, всего пару.

Все дело в том, что код в функциональном стиле часто позволяет судить о корректности сам по себе. Давайте попробуем порассуждать:

Во-первых, рассмотрим случай вызова `Action.compile(restOfTransaction)`:
1. Если `perform` выполнился с ошибкой, то `restOfTransaction` не выполняется (потому что он требует значения типа `T`, которое возвращает `perform` в случае успеха)
2. Порядок выполнения в методе `compile`: сначала `perform`, потом `restOfTransaction` (из тех же соображений обязательности наличия значения `T`)
3. Если `perform` выполнился успешно, то обязательно выполнится либо `commit` либо `compensate` после завершения `restOfTransaction` (по контракту `redeemWith`)

Рассуждая так же по отношению к `ActionChain.compile(restOfTransaction)` легко можно видеть, что все конечные `Action`'ы выстраиваются в цепочку, разделенную `compile`'ами:

```
transaction.compile(restOfTransaction) 
=== 
action1.compile(t1 => 
  action2.compile(t2 =>
    action3.compile(t3 => ...
      restOfTransaction(tn))))
```

например, даже если `ActionChain.first` тоже `ActionChain`:

```
ActionChain(ActionChain(action1, t1 => action2), t2 => action3).compile(restOfTransaction) >>
ActionChain(action1, t1 => action2).compile(t2 => action3.compile(restOfTransaction)) >>
action1.compile(t1 => action2.compile(t2 => action3.compile(restOfTransaction))) []
```

А исходя из свойств `Action.compile` такая цепочка гарантирует выполнение контракта транзакции:
1. Действия выполняются по очереди
2. Если любое действие завершилось ошибкой остальные не выполняются
3. Если любое действие завершилось успешно после завершения остатка обязательно выполнится либо его `commit` либо его `compensate`

</spoiler>

### При чем тут монада?
Проблему мы частично решили:
1. Действия описываются вместе с компенсациями (и с коммитом за одно)
2. Транзакция автоматически собирается в одно действие, которое гарантирует
транзакционность поведения
3. ???
4. PROFIT!

Но если сравнить исходный код решения и финальный видно, что мы все еще имеем пирамиду:
```scala
ActionChain(
  saveDataAction(data),
  { file => 
    ActionChain(
      addFileRefAction(documentId, file),
      { _ => 
        addFileRefAction(fileRegistry, file)
      }
    )
  }
).compile
```
Если мы для удобства сделаем метод `chain` вместо явного использования `ActionChain`, то сможем описать это немного иначе:
```scala
sealed trait Transaction[T] {
  def chain[R](f: T => Transaction[R]): Transaction[R] = ActionChain(this, f)
}

saveDataAction(data).chain { file =>
  addFileRefAction(documentId, file).chain { _ =>
    addFileRefAction(fileRegistry, file)
  }
}.compile
```
Тут внимательный читатель наверняка уже заметил большое сходство метода `chain` и метода `flatMap` во многих структурах, точнее говоря монадах, ведь  именно наличие этого метода (почти) достаточно для существования монады!

Но не будем лезть в дебри теории, нам интересна поддержка монад со стороны Scala, которая заключается в возможности использовать `for` для упрощения кода и поддержка со стороны `cats`, в которой есть много готовых решений для монад (и не только).

Итак, дело за малым - нужно сделать из транзакции настоящую монаду!

```scala
sealed trait Transaction[T] {
  def flatMap[R](f: T => Transaction[R]): Transaction[R] = ActionChain(this, f)
  
  def map[R](f: T => R): Transaction[R] = flatMap { t => Action(IO(f(t))) }
}
```

Этого достаточно для Scala, теперь мы можем писать так:
```scala
def saveDataAndAddFileRefs(data: Data, documentId: ID): Transaction[Unit] = for {
  file <- saveDataAction(data)
  _    <- addFileRefAction(documentId, file)
  _    <- addFileRefAction(fileRegistry, file)
} yield ()

saveDataAndAddFileRefs(data, documentId).compile
```
Вот теперь мы имеем чистый код, явно описывающий намерение - последовательно совершить несколько действий. При этом побочный эффект - транзакционность полностью скрыт, но гарантируется.

Для `cats` нужно явно объявить доказательство того, что `Transaction` это монада -  экземпляр тайпкласса:

```scala
object Transaction {
  implicit object MonadInstance extends Monad[Transaction] {
    override def pure[A](x: A): Transaction[A] = Action(IO.pure(x))

    override def flatMap[A, B](fa: Transaction[A])(f: A => Transaction[B]): Transaction[B] = fa.flatMap(f)

    // этот метод требует cats-effects, для монады в общем он не нужен
    override def tailRecM[A, B](a: A)(f: A => Transaction[Either[A, B]]): Transaction[B] = f(a).flatMap {
      case Left(a) => tailRecM(a)(f)
      case Right(b) => pure(b)
    }
  }
}
```

Что это нам дает? Возможность использовать готовые методы из `cats`, например:
```scala
val transaction = dataChunks.traverse_ { data =>
  saveDataAndAddFileRefs(data, documentId) // выполняет действие для всех элементов списка
}
transaction.compile // собирает все действия в одну транзакцию
```

Весь код доступен тут: https://github.com/atamurius/scala-transactions