import scala.collection.mutable.ListBuffer
//Create sub-classes of parent Exp trait
sealed trait Exp {}
  case class S_boolean(s: String) extends Exp {
    if (s == "#t") {
      true
    }
    else if (s == "#f") {
      false
    }
  }
  sealed trait Atom extends Exp {}
  case class S_List(ls: ListBuffer[Exp]) extends Exp {
    val lb: ListBuffer[Exp] = ls
  }


  case class Number(s: String) extends Atom {
    s.toDouble
    val num_string: String = s
  }

  case class S_Symbol(s2: String) extends Atom {
    val s_string: String = s2
    s_string.toString
  }

  case class S_Prim_Procedure(f1:(List[Exp])=>Exp) extends Exp {
    val function: (List[Exp] => Exp) = f1

    def apply_primitive(args: List[Exp]): Exp = {
      f1(args)
    }
  }
    case class S_Pair(l1:List[Exp]) extends Exp {
      if (l1.length!=2){
        print("Cannot cons more than 2 items")
      }
      Tuple2(l1(0),l1(1))
    }

 class SExp extends Exp with Atom {
   //Environment and Procedure class creation
    class Environment(parms: List[S_Symbol], args: List[Exp], outer: Environment) {
      var immutable_map_env = (parms zip args).toMap
      var map_env = collection.mutable.Map() ++ immutable_map_env

      def find(variable: S_Symbol): Exp = {
        try {
          if (map_env.contains(variable)) {
            map_env(variable)
          }
          else {
            this.outer.map_env(variable)
          }
        }
        catch {
          case no_key: NoSuchElementException =>
            S_Symbol("No_such_variable_exists")
        }
      }


    }

    case class Procedure(parms: List[S_Symbol], body: Exp, env: Environment) extends Exp {
      def my_apply(args: List[Exp]): Exp = {
        var apply_env = new Environment(parms, args, this.env)
        eval(body, apply_env)
      }
    }
//Scanning
    def tokenize(string1: String): ListBuffer[String] = {
      string1.replace("(", " (  ").
        replace(")", " ) ").
        split("\\s+").filter(_ > "").map(_.trim).
        toList.to(ListBuffer)
    }
//Parsing method
   def parse(program: String): Exp = {
      read_from_tokens(tokenize(program))
    }
//Recursively build Abstract Syntax Tree
    def read_from_tokens(tokens: ListBuffer[String]): Exp = {
      if (tokens.length == 0) {
        throw new Exception("Why is this an Unexpected End of Statement")
      }
      var token1 = tokens.remove(0)
      if (token1 == "(") {
        var l1 = ListBuffer[Exp]()
        while (tokens(0) != ")") {

          l1 += read_from_tokens(tokens)
        }
        tokens.remove(0)
        S_List(l1)
      }
      else if (token1 == ")") {
        throw new Exception("Unexpected )")
      }
      else if (token1.length==3 && token1=="("){
        get_token(token1)
      }
      else {
        get_token(token1)
      }
    }
//Get type of individual token: Number or Symbol
    def get_token(s: String): Exp = {
      try {
        Number(s)
      }
      catch {
        case ex: NumberFormatException =>
          S_Symbol(s)
      }
    }
//Base environment creation
    def create_environment(unit: Unit): Environment = {
      val arg_listb=ListBuffer[Exp]()
      val parm_listb=ListBuffer[S_Symbol]()
      val arg_list=arg_listb.toList
      val parm_list=parm_listb.toList
      var base_env = new Environment(parm_list, arg_list, null)
      var sqrt = (n: List[Exp]) => {
        var num_string=""
        var x = n(0)
        x match {
          case Number(s) =>
            var num_arg = Number(s).num_string.toDouble
            num_arg = math.sqrt(num_arg)
            num_string = num_arg.toString
          case S_Symbol(s) => println("Not allowed")
          case S_List(sl)=>println("Can't do this")
          case S_boolean(s1)=>println("Cannot do this")
          case S_Prim_Procedure(l1)=>println("Nope!!")
        }
        Number(num_string)
      }
      base_env.map_env += S_Symbol("sqrt") -> S_Prim_Procedure(sqrt)
      var cons=(n:List[Exp])=>{
        var empty_pair:ListBuffer[Exp]=ListBuffer[Exp]()
        var m1=Number("0.0")
        var m2=S_Symbol("null")
        var empty_buffer:ListBuffer[Exp]=ListBuffer()
        var first_arg=n(0)
        var second_arg=n(1)
        first_arg match{
          case S_Symbol(s)=>
             empty_buffer+=(S_Symbol(s))
          case Number(s)=>
             empty_buffer+=Number(s)
          case S_List(sl)=>
             empty_buffer+=S_List(sl)
          case S_Pair(l1)=>
            empty_buffer+=S_Pair(l1)
          case Procedure(parms,body,env)=>print("Cannot cons a procedure")
          case S_Prim_Procedure(f1)=>print("Cannot cons a procedure")
        }
        second_arg match{
          case S_Symbol(s)=>
             empty_buffer+=S_Symbol(s)
          case Number(s)=>
             empty_buffer+=Number(s)
          case S_List(sl)=>
             empty_buffer+==S_List(sl)
          case S_Prim_Procedure(f1)=>print("Cannot cons a procedure")
          case Procedure(parms,body,env)=>print("Cannot cons a procedure")
          case S_Pair(s1)=>empty_buffer+=S_Pair(s1)
        }
        var new_pair=empty_buffer.toList
        S_Pair(new_pair)
      }
      base_env.map_env+=S_Symbol("cons")->S_Prim_Procedure(cons)
      var cos = (n: List[Exp]) => {
        var x = n(0)
        var cos_string=""
        x match {
          case Number(s) =>
            val n_arg = Number(s).num_string.toDouble
            val cos_val = math.cos(n_arg)
            cos_string = cos_val.toString
            Number(cos_string)
          case S_Symbol(s) =>
            var binding=base_env.map_env(S_Symbol(s))
            binding match {
              case Number(s) =>
                val n_arg2 = Number(s).num_string.toDouble
                val cos_val2 = math.cos(n_arg2)
                cos_string = cos_val2.toString
              case S_Symbol(s2) => print("Cannot find cos of a symbol")
              case S_List(s2) => print("Cannot find cos of a List")
              case S_boolean(s) => print("Cannot find s_boolean of a list")
              case S_Prim_Procedure(l1) => println("Cannot get cos of procedure!!")
            }
          case S_List(sl)=>println("Can't get cos of a list")
          case S_boolean(s1)=>println("Cannot get cos of a boolean")
          case S_Prim_Procedure(l1)=>println("Cannot get boolean of a Procedure")
          case S_Pair(s)=>print("Cannot get cos of a pair")
          case Procedure(parms,body,env)=>print("Cannot get cos of a Procedure")
        }
        Number(cos_string)
      }
      base_env.map_env += S_Symbol("cos") -> S_Prim_Procedure(cos)
      var sin = (num: List[Exp]) => {
          var x=num(0)
              var sin_string=""
              x match {
                case Number(s) =>
                  val n_arg = Number(s).num_string.toDouble
                  val sin_val = math.sin(n_arg)
                  sin_string = sin_val.toString
                case S_Symbol(s)=>
                  var binding=base_env.map_env(S_Symbol(s))
                  binding match {
                    case Number(s) =>
                      val n_arg2 = Number(s).num_string.toDouble
                      val sin_val2 = math.sin(n_arg2)
                      sin_string = sin_val2.toString
                    case S_Symbol(s2) => print("Cannot find sin of a symbol")
                    case S_List(s2) => print("Cannot find sin of a List")
                    case S_boolean(s) => print("Cannot find s_boolean of a list")
                    case S_Prim_Procedure(l1) => println("Cannot get sin of procedure!!")
                  }
                case S_List(sl)=>println("Can't do this")
                case S_boolean(s1)=>println("Cannot get sin of a boolean")
                case S_Prim_Procedure(l1)=>println("Cannot get sin of procedure!!")
              }
              println(sin_string)
              Number(sin_string)
            }
      base_env.map_env += S_Symbol("sin") -> S_Prim_Procedure(sin)
      var > = (nums: List[Exp]) => {
        var ans = S_boolean("#t")
        if (nums.length == 1) {
          throw new Exception("Need at least 2 numbers to check if greater than")
        }
        var x=nums(0)
        var first=0.0
        x match {
          case Number(s) =>
             first = Number(s).num_string.toDouble
          case S_Symbol(s) =>
            var binding=base_env.map_env(S_Symbol(s))
            binding match{
              case Number(s)=>
                first=Number(s).num_string.toDouble
            }
          case S_List(sl)=>println("Can't do this")
          case S_boolean(s1)=>println("Cannot do this")
          case S_Prim_Procedure(l1)=>println("Can't check procedure equality")
          case S_Pair(s)=>print("Cannot check pair equality")
          case Procedure(parms,body,env)=>print("Cannot check procedure equality")
        }
        val sliced = nums.slice(1, nums.length)
        for (num <- sliced) {
          num match {
            case Number(s) =>
              var nn = Number(s).num_string.toDouble
              if (first <= nn) {
                ans = S_boolean("#f")
              }
            case S_Symbol(s) =>
              var num=("0")
              var symbol_num=(base_env.map_env(S_Symbol(s)))
              symbol_num match{
                case Number(s)=>
                  num=Number(s).num_string
                  var num_sym=num.toDouble
                  if (first<=num_sym){
                    ans=S_boolean("#f")
                  }
              }
            case S_List(sl)=>println("Can't do this")
            case S_boolean(s1)=>println("Cannot do this")
            case S_Prim_Procedure(l1)=>println("Cannot check procedure equality")
            case S_Pair(s)=>print("Cannot check pair equality")
            case Procedure(parms,body,env)=>print("Cannot check procedure equality")
          }
        }
        println(ans)
        ans
      }
      base_env.map_env += S_Symbol(">") -> S_Prim_Procedure(>)
      var < = (nums: List[Exp]) => {
        var ans = S_boolean("#t")
        if (nums.length == 1) {
          throw new Exception("Need at least 2 numbers to check if greater than")
        }
        var x=nums(0)
        var first=0.0
        var sliced=nums.slice(1,nums.length)
        x match{
          case Number(s)=>
            first=Number(s).num_string.toDouble
          case S_Symbol(s) =>
            var binding=base_env.map_env(S_Symbol(s))
            binding match{
              case Number(s)=>
                first=Number(s).num_string.toDouble
            }
          case S_List(sl)=>println("Can't do this")
          case S_boolean(s1)=>println("Cannot do this")
          case S_Prim_Procedure(l1)=>println("Cannot check procedure equality")
          case Procedure(parms,body,env)=>println("Cannot check Procedure equality")
        }
        for (num <- sliced) {
          num match {
            case Number(s) =>
              var nn = Number(s).num_string.toDouble
              if (first >= nn) {
                ans = S_boolean("#f")
              }
            case S_Symbol(s) =>
              var num=("0")
              var symbol_num=(base_env.map_env(S_Symbol(s)))
              symbol_num match{
                case Number(s)=>
                  num=Number(s).num_string
                  var num_sym=num.toDouble
                  if (first>=num_sym){
                    ans=S_boolean("#f")
                  }
              }
            case S_List(sl)=>println("Can't compare Lists like this")
            case S_boolean(s1)=>println("Can't check boolean size")
            case S_Prim_Procedure(l1)=>println("Can't comapre Primitive procedures like this")
            case Procedure(parms,body,env)=>print("Can't compare procedures like this")
            case S_Pair(ls)=>print("Cannot check pair sizes like this")
          }
        }
        println(ans)
        ans
      }
      base_env.map_env += S_Symbol("<") -> S_Prim_Procedure(<)
      var >= = (nums2: List[Exp]) => {
        var ans = S_boolean("#t")
        if (nums2.length == 1) {
          throw new Exception("Need at least 2 numbers to check if greater than")
        }
        var first = 0.0
        var x=nums2(0)
        x match{
          case Number(s)=>
            first=Number(s).num_string.toDouble
          case S_Symbol(s) =>
            var num=("0")
            var symbol_num=(base_env.map_env(S_Symbol(s)))
            symbol_num match{
              case Number(s)=>
                first=Number(s).num_string.toDouble
        }
          case S_List(sl)=>S_List(sl)
          case S_boolean(s1)=>println("Cannot compare sizes of booleans")
          case S_Prim_Procedure(l1)=>println("Wrong Input Type")
          case Procedure(parms,body,env)=>println("Cannot compare Procedure size")
        }
        val sliced = nums2.slice(1, nums2.length)
        for (num <- sliced) {
          num match {
            case Number(s) =>
              var nn = Number(s).num_string.toDouble
              if (first < nn) {
                ans = S_boolean("#f")
              }
            case S_Symbol(s)=>
              var num=("0")
              var symbol_num=(base_env.map_env(S_Symbol(s)))
              symbol_num match{
                case Number(s)=>
                num=Number(s).num_string
                var num_sym=num.toDouble
                  if (first<num_sym){
                    ans=S_boolean("#f")
          }
          }
            case S_List(sl)=>println("Can't check list equality")
            case S_boolean(s1)=>println("Cannot do this")
            case S_Prim_Procedure(l1)=>println("Cannot check Procedure equality")
          }
        }
        println(ans)
        ans
      }
      base_env.map_env += S_Symbol(">=") -> S_Prim_Procedure(>=)
      var <= = (nums: List[Exp]) => {
        var ans = S_boolean("#t")
        var first=0.0
        val x=nums(0)
        if (nums.length == 1) {
          throw new Exception("Need at least 2 numbers to check if greater than")
        }
        x match{
          case Number(s)=>first=Number(s).num_string.toDouble
          case S_Symbol(s)=>
            var num=("0")
            var symbol_num=(base_env.map_env(S_Symbol(s)))
            symbol_num match{
              case Number(s)=>
                first=Number(s).num_string.toDouble
        }
          case S_List(sl)=>println("Can't check list equality")
          case S_boolean(s1)=>S_boolean(s1)
          case S_Prim_Procedure(l1)=>println("Cannot check procedure equality")
          case S_Pair(s)=>print("Cannot check pair equality")
          case Procedure(parms,body,env)=>print("Cannot check procedure equality")
        }
        val sliced = nums.slice(1, nums.length)
        for (num <- sliced) {
          num match {
            case Number(s) =>
              var nn = Number(s).num_string.toDouble
              if (first > nn) {
                ans = S_boolean("#f")
              }
            case S_Symbol(s)=>
              var num=("0")
             var symbol_num=(base_env.map_env(S_Symbol(s)))
            symbol_num match{
              case Number(s)=>
              num=Number(s).num_string
              var num_sym=num.toDouble
              if (first>num_sym){
                ans=S_boolean("#f")
          }
          }
            case S_List(sl)=>println("Can't do this")
            case S_boolean(s1)=>println("Cannot do this")
            case S_Prim_Procedure(l1)=>println("Nope!!")
            case S_Pair(s)=>print("Cannot check pair equality")
            case Procedure(parms,body,env) =>print("Cannot check procedure equality")
          }
        }
        println(ans)
        ans
      }
      base_env.map_env += S_Symbol("<=") -> S_Prim_Procedure(<=)
      var == = (nums: List[Exp]) => {
        var ans = S_boolean("#t")
        var first=0.0
        if (nums.length == 1) {
          throw new Exception("Need at least 2 numbers to check if greater than")
        }
        val x=nums(0)
        x match{
          case Number(s)=>first=Number(s).num_string.toDouble
          case S_Symbol(s)=>
            var symbol_num=(base_env.map_env(S_Symbol(s)))
            symbol_num match{
              case Number(s)=>
                var num=Number(s).num_string
                first=num.toDouble
            }
          case S_List(sl)=>println("Can't check list equality")
          case S_boolean(s1)=>S_boolean(s1)
          case S_Prim_Procedure(l1)=>println("Can't check procedure equality")
          case Procedure(parms,body,env)=>print("Cannot check procedure equality")
        }
        val sliced = nums.slice(1, nums.length)
        for (num <- sliced) {
          num match {
            case Number(s) =>
              var nn = Number(s).num_string.toDouble
              if (first != nn) {
                ans = S_boolean("#f")
              }
            case S_Symbol(s)=>
              var num=("0")
              var symbol_num=(base_env.map_env(S_Symbol(s)))
              symbol_num match{
                case Number(s)=>
                  num=Number(s).num_string
                  var num_sym=num.toDouble
                  if (first!=num_sym){
                    ans=S_boolean("#f")
                  }
              }
            case S_List(sl)=>println("Cannot check list equality")
            case S_boolean(s1)=>S_boolean(s1)
            case S_Prim_Procedure(l1)=>println("Cannot check Primitive Procedure equality")
            case S_Pair(l1)=>println("Cannot check Pair equality")
            case Procedure(parms,body,env)=>print("Cannot check Procedure equality")
          }
        }
        println(ans)
        ans
      }
      base_env.map_env += S_Symbol("=") -> S_Prim_Procedure(==)
      var car = (sl:List[Exp]) => {
        val x=sl(0)
        var first:Exp=S_Symbol("")
        x match{
          case S_List(ls)=>first=ls(1)
          case S_Symbol(s)=>print("Must be a list or pair")
          case Number(s)=>println("Must be a list or pair")
          case S_boolean(s1)=>println("Must be a list or pair")
          case S_Prim_Procedure(l1)=>println("Must be a list or pair")
          case S_Pair(l1)=>l1(0)
        }
        first
      }
      base_env.map_env += S_Symbol("car") -> S_Prim_Procedure(car)
      var append= (ap:List[Exp])=>{
        var firstlist=ap(0)
        var secondlist=ap(1)
        var first_append:ListBuffer[Exp]=ListBuffer()
        var second_append:ListBuffer[Exp]=ListBuffer()
        firstlist match{
          case S_List(ls)=>first_append=S_List(ls).lb.slice(1,S_List(ls).lb.length)
          case S_Symbol(s2)=>
            var check_me=base_env.map_env(S_Symbol(s2))
            check_me match{
              case S_List(ls)=>first_append=S_List(ls).lb.slice(1,S_List(ls).lb.length)
            }
          case Number(s3)=>print("Cannot append number")
          case Procedure(parms,body,env)=>print("Cannot append proc")
          case S_Prim_Procedure(f1)=>print("Cannot append proc")

        }
        secondlist match{
          case S_List(ls)=>second_append=S_List(ls).lb.slice(1,ls.length)
          case S_Symbol(s2)=>
            var check_me=base_env.map_env(S_Symbol(s2))
            check_me match{
              case S_List(ls)=>second_append=S_List(ls).lb.slice(1,ls.length)
            }
          case Number(s3)=>print("Cannot append number")
          case Procedure(parms,body,env)=>print("Cannot append proc")
          case S_Prim_Procedure(f1)=>print("Cannot append proc")
        }
        val d=ListBuffer.concat(first_append,second_append)
        S_List(d)
      }
      base_env.map_env+= S_Symbol("append")->S_Prim_Procedure(append)
      var cdr = (sl: List[Exp]) => {
        var x=sl(0)
        var cdr_list:ListBuffer[Exp]=ListBuffer()
        //var cdr_list:Exp=S_List(empty)
        x match{
          case S_List(ls)=>cdr_list=ls.slice(2,ls.length)
          case S_Symbol(s)=>print("Must be a list or pair")
          case Number(s)=>println("Must be a list or pair")
          case S_boolean(s1)=>println("Must be a list or pair ")
          case S_Prim_Procedure(l1)=>println("Must be a list")
          case S_Pair(l1)=>l1(1)
        }
        S_List(cdr_list)
      }
      base_env.map_env += S_Symbol("cdr") -> S_Prim_Procedure(cdr)
      var List = (exp: List[Exp]) => {
        var new_list = ListBuffer[Exp]()
        for (ex <- exp) {
          new_list += ex
        }
        var new_slist = S_List(new_list)
        new_slist
      }
      base_env.map_env += S_Symbol("List") -> S_Prim_Procedure(List)
      base_env
    }
//Evaluation
    def eval(x: Exp, env: Environment): Exp= x match {
        case Number(s) =>
          Number(s)
        case S_Symbol(s2) =>
          try {
            env.find(S_Symbol(s2))
          }
          catch{
            case no_binding:NullPointerException=>S_Symbol(s2)
          }
        case S_boolean(s) => S_boolean(s)
        case S_List(ls) =>
          var first = ls(1)
          val prim_checker=ls(0)
          if (ls(0) == S_Symbol("define")) {
            var variable_bind = S_Symbol("Placeholder")
            first match {
              case S_Symbol(s) =>
                variable_bind = S_Symbol(s)
              case Number(s) => println("Variable must be a symbol")
              case S_List(sl) => println("Variable cannot be a List")
              case S_boolean(s1) => println("Variable cannot be a boolean")
              case S_Prim_Procedure(l1) => println("Variable cannot be a Primitive Procedure")
            }
            var value_to_bind: Exp = eval(ls(2), env)
            env.map_env += variable_bind -> value_to_bind
            S_Symbol("Binding created")
          }
          else if (ls(0) == S_Symbol("display")) {
            print("Printed output is" + " " + eval(ls(1),env))
            S_Symbol("Ok")
          }
          else if (ls(0)==S_Symbol("if")){
            var test=eval(ls(1),env)
            var consequent=eval(ls(2),env)
            var alt=eval(ls(3),env)
            if (test==S_boolean("#t")){
              consequent
            }
            else{
              alt
            }
          }
          else if (ls(0) == S_Symbol("set!")) {
            var reset_variable = S_Symbol("placeholder")
            first match {
              case S_Symbol(s) => reset_variable = S_Symbol(s)
              case Number(s) => println("Variable must be a symbol")
              case S_List(sl) => println("Variable cannot be an S-List")
              case S_boolean(s1) => println("Variable cannot be a boolean")
              case S_Prim_Procedure(l1) => println("Variable cannot be a Primitive Procedure")
              case S_Pair(s)=>println("Variable cannot be an S-Pair")
            }
            var reset_to_this = eval(ls(2), env)
            var updated_map=env.map_env.updated(reset_variable,reset_to_this)
            env.map_env=updated_map
            S_Symbol("Binding has been reset")
          }
          else if (ls(0)==S_Symbol("quote")){
            S_Symbol("'"+eval(ls(1),env).toString)
          }
          else if (ls(0)==S_Symbol("and")){
            var default_bool=S_boolean("#t")
            var testers=ls.slice(1,ls.length)
            for (test<-testers){
              if (eval(test,env)==S_boolean("#f")){
                default_bool=S_boolean("#f")
              }
            }
            default_bool
          }
          else if (ls(0)==S_Symbol("+")){
            if (ls.length == 2) {
              throw new Exception("Need at least two numbers to add")
            }
            var sum = 0.0
            var adders=ls.slice(1,ls.length)
            for (num <- adders) {
              num match {
                case Number(s2) =>
                  var ns = Number(s2).num_string
                  var ns_num = ns.toDouble
                  sum = sum + ns_num
                case S_Symbol(s) =>
                  var num=("0")
                  var symbol_num=(env.map_env(S_Symbol(s)))
                  symbol_num match{
                    case Number(s)=>
                      num=Number(s).num_string
                      var num_sum=num.toDouble
                      sum=sum+num_sum
                  }
                case S_List(sl)=>
                  var num_add_eval=eval(S_List(sl),env)
                  num_add_eval match {
                    case Number(s) =>
                      var num_list_add = Number(s).num_string.toDouble
                      sum=sum+num_list_add
                  }
                case S_boolean(s1)=>println("Cannot do this")
                case S_Prim_Procedure(l1)=>println("Cannot add to list")
              }
            }
            val sum_string = sum.toString
            Number(sum_string)
          }
          else if (ls(0)==S_Symbol("*")){
            if (ls.length == 2) {
              throw new Exception("Need at least two numbers to multiply")
            }
            var product = 1.0
            var multipliers=ls.slice(1,ls.length)
            for (num <- multipliers) {
              num match {
                case Number(s2) =>
                  var ns = Number(s2).num_string
                  var ns_num = ns.toDouble
                  product = product * ns_num
                case S_Symbol(s) =>
                  var num=("0")
                  var symbol_num=(env.map_env(S_Symbol(s)))
                  symbol_num match{
                    case Number(s)=>
                      num=Number(s).num_string
                      var num_sum=num.toDouble
                      product=product*num_sum
                  }
                case S_List(sl)=>
                  var num_mult_eval=eval(S_List(sl),env)
                  num_mult_eval match {
                    case Number(s) =>
                      var num_mult = Number(s).num_string.toDouble
                      product=product*num_mult
                  }
                case S_boolean(s1)=>println("Cannot do this")
                case S_Prim_Procedure(l1)=>println("Cannot do this!!")
              }
            }
            val product_string = product.toString
            Number(product_string)
          }
           else if (ls(0)==S_Symbol("/")){
            var quotient=0.0
            var x=ls(1)
            x match {
              case Number(s3) =>
                quotient = Number(s3).num_string.toDouble
              case S_Symbol(s) =>
                var num=("0")
                var symbol_num=(env.map_env(S_Symbol(s)))
                symbol_num match{
                  case Number(s)=>
                    num=Number(s).num_string
                    var num_quot=num.toDouble
                    quotient=num_quot.toDouble
                }
              case S_List(sl)=>
                var num_quot_eval=eval(S_List(sl),env)
                num_quot_eval match {
                  case Number(s) =>
                    quotient = Number(s).num_string.toDouble
                }
              case S_boolean(s1)=>println("Cannot do this")
              case S_Prim_Procedure(l1)=>println("Nope!!")
            }
            var dividers = ls.slice(2, ls.length)
            if (ls.length == 2) {
              throw new Exception("Need at least two numbers to add")
            }
            for (num <- dividers) {
              num match {
                case Number(s) =>
                  var ns = Number(s).num_string.toDouble
                  quotient = quotient / ns
                case S_Symbol(s) =>
                  var num=0.0
                  var symbol_num=(env.map_env(S_Symbol(s)))
                  symbol_num match{
                    case Number(s)=>
                      num=Number(s).num_string.toDouble
                      quotient=quotient/num
                  }
                case S_List(sl)=>eval(S_List(sl),env)
                case S_boolean(s1)=>println("Cannot divide booleans")
                case S_Prim_Procedure(l1)=>println("Wrong format")
              }
            }
            var quotient_string = quotient.toString
            Number(quotient_string)
          }
          else if (ls(0)==S_Symbol("or")) {
            var bool_val=S_boolean("#t")
            var testers = ls.slice(1, ls.length)
            for (test <- testers) {
              if (eval(test, env) == S_boolean("#t")) {
                return S_boolean("#t")
              }
            }
            S_boolean("#f")
          }
          else if (ls(0)==S_Symbol("-")){
          var difference=0.0
          var x=ls(1)
          x match {
            case Number(s) =>
              difference = Number(s).num_string.toDouble
            case S_List(sl)=>
              var subexpression_sub=eval(S_List(sl),env)
              subexpression_sub match{
                case Number(s)=>difference=Number(s).num_string.toDouble
                case S_Symbol(s)=>print("Cannot add symbols")
                case S_Pair(s)=>print("Cannot add pairs")
                case S_boolean(s)=>print("Cannot add booleans")
                case S_Prim_Procedure(f1)=>print("Cannot add procs")
                case Procedure(parms,body,env)=>print("Cannot add procs")
              }
            case S_boolean(s1)=>println("Cannot add booleans")
            case S_Prim_Procedure(l1)=>println("Cannot add procs")
            case Procedure(parms,body,env)=>print("Cannot add procedures")
            case S_Pair(s)=>print("Cannot add pairs")
            case S_Symbol(s)=>
              var num_diff3=0.0
              var symbol_num=(env.map_env(S_Symbol(s)))
              symbol_num match{
                case S_boolean(s1)=>println("Cannot add booleans")
                case S_Prim_Procedure(l1)=>println("Cannot add procs")
                case Procedure(parms,body,env)=>print("Cannot add procedures")
                case S_Pair(s)=>print("Cannot add pairs")
                case Number(s)=>
                  var num=Number(s).num_string.toDouble
                  var num_diff3=num.toDouble
              }

              Number(num_diff3.toString)
          }
          var subtractors = ls.slice(2, ls.length)
          for (num <- subtractors) {
            num match {
              case Number(s) =>
                var ns = Number(s).num_string.toDouble
                difference = difference - ns
              case S_Symbol(s) =>
                var num=("0")
                var symbol_num=(env.map_env(S_Symbol(s)))
                symbol_num match{
                  case Number(s)=>
                    num=Number(s).num_string
                    var difference2=num.toDouble
                    difference=difference-difference2
                }
              case S_List(sl)=>
                var subexpression_sub2=eval(S_List(sl),env)
                subexpression_sub2 match{
                  case Number(s)=>
                    var num2=Number(s).num_string
                    var difference3=num2.toDouble
                    difference=difference-difference3
                }
              case S_boolean(s1)=>println("Cannot add booleans")
              case S_Prim_Procedure(l1)=>println("Cannot add procs")
              case Procedure(parms,body,env)=>print("Cannot add procedures")
              case S_Pair(s)=>print("Cannot add pairs")
            }
          }
          val difference_string = difference.toString
          Number(difference_string)
    }
          else if (ls(0) == S_Symbol("lambda")) {
            var proc_parms = ListBuffer[S_Symbol]()
            first match {
              case S_List(sl) =>
                for (symbols <- sl) {
                  symbols match {
                    case S_Symbol(s) => proc_parms += S_Symbol(s)
                    case S_List(sl) => println("Can't do this")
                    case S_boolean(s1) => println("Cannot do this")
                    case S_Prim_Procedure(l1) => println("Invalid Fomat")
                    case Number(s) => ("Incorrect Lambda Format")
                  }
                }
            }
            val proc_parms_list = proc_parms.toList
            var proc_body = ls(2)
            var new_proc = new Procedure(proc_parms_list, proc_body, env)
            print("#<procedure>")
            return new_proc
          }
          else {
            prim_checker match {
              case S_Symbol(s2) =>
                var value_of_proc = env.map_env(S_Symbol(s2))
                value_of_proc match {
                  case S_Prim_Procedure(f1) =>
                     S_Prim_Procedure(f1).apply_primitive(ls.slice(1, ls.length).toList)
                  case Procedure(parms,body,env)=>Procedure(parms,body,env).my_apply(ls.slice(1,ls.length).toList)
                  }
                }
            }
        case S_Prim_Procedure(f1)=>S_Symbol("Prim_Proc")
        case Procedure(parms,body,env)=>S_Symbol("Procedure")
          }
      }

val test=new SExp()
val global=test.create_environment()
var test_val5=test.eval(test.parse("(define z (lambda (x) (* x 2)))"),global)
var test_val6=test.eval(test.parse("(z 6)"),global)
var test_val7=test.eval(test.parse("(define m 3)"),global)
var test_val8=test.eval(test.parse("(set! m 9)"),global)
var test_val9=test.eval(test.parse("(display m)"),global)
var test_val10=test.eval(test.parse("(* (+ m 10) (/ 6 3))"),global)
var test_val11=test.eval(test.parse("(if (< m 3) (* 10 9) (* 11 23))"),global)
var test_val12=test.eval(test.parse("(cons 3 9)"),global)
var test_val13=test.eval(test.parse("(List 1 2 3 4)"),global)
var test_val14=test.eval(test.parse("(car(List 1 2 3 4))"),global)
var test_val15=test.eval(test.parse("(cdr(List 1 2 3 4))"),global)
var test_val16=test.eval(test.parse("(- (+ 9 7) (* 11 9))(- (+ 9 7) (* 11 9))"),global)
var test_val16=test.eval(test.parse("(append (List 1 2 3) (List 3 4 5))"),global)
