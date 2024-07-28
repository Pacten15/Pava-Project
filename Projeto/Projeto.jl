import Base.show


#########################################################
# Defined Structs For The Metajulia Language
#########################################################

# Define a struct to represent a variable
struct variable
    name::Symbol
    value::Any
    isPrimitive::Bool
    variable(name, value) = new(name, value, false)
    variable(name, value, isPrimitive) = new(name, value, isPrimitive)
end

# Define a struct to represent an environment
struct environment
    data::Array{variable}
    prev::Union{environment,Nothing}
end

# Define a struct to represent a method definition
struct method_def
    types::Array{Symbol}
    block::Expr
end

# Define a struct to represent a function definition
struct function_def
    args::Array{Symbol}
    methods::Array{method_def}
    env::environment
    function_def(args, block, env) = new(args, [method_def(repeat([:Any], length(args)), block)], env)
    function_def(args, block, env, types) = new(args, [method_def(types, block)], env)
end

# Define a struct to represent a function expression (fexpr)
struct fexpr_def
    args::Array{Symbol}
    block::Expr
    env::environment
end


# Define a struct to represent a macro definition
struct macro_def
    args::Array{Symbol}
    block::Expr
    env::environment
end

################################################################
# Custom Show Method For Printing Variables In A Readable Format
################################################################

show(io::IO, func::Function) = print(io, "<function>")
show(io::IO, func::function_def) = print(io, "<function>")
show(io::IO, fexpr::fexpr_def) = print(io, "<fexpr>")
show(io::IO, method::method_def) = print(io, "<method>")
show(io::IO, macro_expr::macro_def) = print(io, "<macro>")


#########################################################
#Auxiliary Function For Environments
#########################################################

# Function to create a new environment
function create_environment(env)
    return environment([], env)
end



#########################################################
#Auxiliary Functions For Variables
#########################################################

# Function to create a symbol from a given name
function make_symbol(name)
    return Symbol(name)
end

# Function to extract the name of a variable
function variable_name(expr)
    return Symbol(expr.args[1])
end

# Function to extract the value of a variable
function variable_value(expr)
    return expr.args[2]
end

# Function to create a primitive variable
function make_primitive(func)
    return variable(make_symbol(func), func, true)
end

# Function to determine if a variable is primitive
function primitive(var)
    return var.isPrimitive
end


# Function to find the index of a variable in an environment
function find_variable_env_index(name, env)
    for i in eachindex(env.data)
        if make_symbol(name) == env.data[i].name
            return i
        end
    end
    return -1
end


#########################################################
#Auxiliary Functions For General Function Expressions
#########################################################

# Function to extract the head of a function expression
function function_head(expr)
    return expr.args[1]
end

# Function to extract the name of a function
function function_name(expr)
    return Symbol(expr.args[1])
end

# Function to extract the arguments of a function
function function_arguments(expr)
    let args = []
        for i in 2:length(expr.args)
            if typeof(expr.args[i]) == Expr
                push!(args, Symbol(expr.args[i].args[1]))
            else
                push!(args, Symbol(expr.args[i]))
            end
        end
        return args
    end
end

# Function to extract the argument types of a function
function function_argument_types(expr)
    let types = []
        for i in 2:length(expr.args)
            if typeof(expr.args[i]) == Expr
                push!(types, expr.args[i].args[2])
            else
                push!(types, :Any)
            end
        end
        return types
    end
end

# Function to extract arguments from a function call
function function_call_arguments(expr)
    let args = []
        for i in 2:length(expr.args)
            push!(args, expr.args[i])
        end
        return args
    end
end

# Function to extract the block of a function
function function_block(expr)
    return expr.args[2]
end

# Function to determine if an expression represents a function definition
function function_definition(expr)
    return typeof(expr.args[2]) == function_def
end


#########################################################
# Methods Implementation
#########################################################


# Function to create a method definition
function methods(func)
    if typeof(func) == function_def
        let methodList = "Methods:" * "\n"
            for i in eachindex(func.methods)
                methodList *= "[" * string(i) * "] = ["
                for j in eachindex(func.methods[i].types)
                    methodList *= string(func.args[j]) * "::" * string(func.methods[i].types[j])
                    if j != length(func.methods[i].types) 
                        methodList *= ", "
                    end
                end
                methodList *= "]\n"
            end
            return methodList
        end
    end
    return "This variable is not a function -- Methods"
end

# Function to create a method definition
function methodAlreadyExists(methods, types)
    let argSize = length(types)
        for i in eachindex(methods)
            let numMatch = 0
                for j in eachindex(methods[i].types)
                    if methods[i].types[j] == types[j]
                        numMatch += 1
                    end
                end
                if argSize == numMatch
                    return i
                end
            end
        end
    end
    return -1
end

# Function to check if a method exists
function methodExists(methods, types)
    let argSize = length(types), anyMethod = -1
        for i in eachindex(methods)
            let numAny = 0, numMatch = 0
                for j in eachindex(methods[i].types)
                    if methods[i].types[j] == types[j]
                        numMatch += 1
                    elseif methods[i].types[j] == :Any
                        numAny += 1
                    end
                end
                if argSize == numMatch
                    return i
                elseif argSize == numAny
                    anyMethod = i
                end
            end
        end
        return anyMethod
    end
end

#########################################################
#Functions To Verify The Type Of An Expression
#########################################################


# Function to check if an expression is quoted
function quote_form(expr)
    return typeof(expr) == QuoteNode || expr.head == Symbol("quote")
end

# Function to check if an expression is a symbol
function name(expr)
    return typeof(expr) == Symbol
end

# Function to determine if an expression represents a variable assignment
function variable_assignment(expr)
    return typeof(expr.args[2]) != function_def && 
            (typeof(expr.args[2]) != Expr || 
                (expr.args[2].head != Symbol("block") && 
                    expr.args[2].head != Symbol("let")))
end

# Function to check if an expression is a global expression
function global_expr(expr)
    return expr.head == Symbol("global")
end

# Function to check if an expression is a let expression
function let_expr(expr)
    return expr.head == Symbol("let")
end

# Function to check if an expression is a function call
function function_call(expr)
    return expr.head == Symbol("call")
end

# Function to check if an expression represents a call to an fexpr
function fexpr_call(expr)
    return typeof(expr) == fexpr_def
end

# Function to check if an expression represents a call to a macro
function macro_call(expr)
    return typeof(expr) == macro_def
end


# Function to check if an expression is a if expression
function if_expr(expr)
    return expr.head == Symbol("if") || expr.head == Symbol("elseif")
end

# Function to check if an expression represents a logical OR operation
function or(expr)
    return expr.head == Symbol("||")
end

# Function to check if an expression represents a logical AND operation
function and(expr)
    return expr.head == Symbol("&&")
end

# Function to check if an expression represents a block
function block(expr)
    return expr.head == Symbol("block")
end

# Function to check if an expression represents an assignment
function assignment(expr)
    return expr.head == Symbol("=")
end

# Function to check if an expression represents an anonymous function
function ifAnonymous(expr)
    return expr.head == Symbol("->")
end

# Function to check if an expression represents an anonymous function call
function anonymous(expr)
    return typeof(expr.args[1]) == Expr && expr.args[1].head == Symbol("->")
end

# Function to check if an expression represents an fexpr
function fexpr(expr)
    return expr.head == Symbol(":=")
end

# Function to check if an expression represents a macro
function ismacro(expr)
    return expr.head == Symbol("\$=")
end

# Function to check if an expression represents a macro call
function dollar(expr)
    return expr.head == Symbol("\$")
end

#########################################################
#Fexpr Eval Implementation
#########################################################

# Function to evaluate an expression in a given environment
function scoped_eval(expr, env)
    return eval(expr, env)
end

# Function to extract the block from an fexpr
function fexpr_block(expr)
    if name(expr.args[2])
        return Expr(:block, expr.args[2])
    end
    return expr.args[2]
end

# Function to initialize the initial bindings for fexprs in an environment
function initial_bindings_fexpr(env)
    let fexpr_env = create_environment(env)
        pushfirst!(fexpr_env.data, variable(:eval, (expr) -> scoped_eval(expr, fexpr_env), true))
        return fexpr_env
    end
end

# Function to evaluate an fexpr
function eval_fexpr(expr, env)
    let fexpr_env = initial_bindings_fexpr(env)
        let head = function_head(expr)
            let name = function_name(head), args = function_arguments(head), block = fexpr_block(expr)
                let index = find_variable_env_index(name, env), fexpr = fexpr_def(args, block, fexpr_env)
                    if index != -1
                        env.data[index] = variable(name, fexpr)
                    else
                        pushfirst!(env.data, variable(name, fexpr))
                    end
                    return fexpr
                end
            end
        end
    end
end

#########################################################
#Macro Eval Implementation
#########################################################

# Function to extract the head of a macro expression
function macro_head(expr)
    return expr.args[1]
end

# Function to extract the name of a macro
function macro_name(expr)
    return macro_head(expr).args[1]
end

# Function to extract the quoted block of a macro
function macro_quote(expr)
    return expr.args[2]
end

# Function to extract the block of a macro
function macro_block(expr)
    return macro_quote(expr).args[1]
end

# Function to extract the assignment expression of a macro
function macro_assigment(expr)
    return expr.args[2].args[1]
end

# Function to extract the block of an assignment in a macro
function macro_assigment_block(expr)
    return expr.args[2].args[2].args[2].args[1]
end

# Function to evaluate a macro definition
function eval_macro(expr, env)
    let macro_name = macro_name(expr), macro_args = function_arguments(macro_head(expr))
        let  macro_block = macro_block(expr), macro_env = create_environment(env)
            if !quote_form(macro_quote(expr))
                eval(macro_assigment(expr), macro_env)
                macro_block = macro_assigment_block(expr)
            end
            let macro_def = macro_def(macro_args, macro_block, macro_env), index = find_variable_env_index(macro_name, env)
                if index != -1
                    env.data[index] = variable(macro_name, macro_def)
                else
                    pushfirst!(env.data, variable(macro_name, macro_def))
                end
                return macro_def
            end
        end
    end
end

#########################################################
#Conditional Eval Expressions
#########################################################

# Function to extract the first logical argument of an expression
function first_logical_argument(expr)
    return expr.args[1]
end

# Function to extract the second logical argument of an expression
function second_logical_argument(expr)
    return expr.args[2]
end

# Function to extract the first operation argument of an expression
function first_operation_argument(expr)
    return expr.args[2]
end

# Function to extract the second operation argument of an expression
function second_operation_argument(expr)
    return expr.args[3]
end

# Function to extract the condition of an if expression
function if_condition(expr)
    return expr.args[1]
end

# Function to extract the consequent of an if expression
function if_consequent(expr)
    return expr.args[2]
end

# Function to extract the alternative of an if expression
function if_alternative(expr)
    return expr.args[3]
end


# Function to evaluate a logical OR operation
function eval_or(expr, env)
    let result = eval(first_logical_argument(expr), env)
        if result != false
            return result
        else
            return eval(second_logical_argument(expr), env)
        end
    end
end

# Function to evaluate a logical AND operation
function eval_and(expr, env)
    let result = eval(first_logical_argument(expr), env)
        if result != false
            return eval(second_logical_argument(expr), env)
        end
        return result
    end
end

# Function to evaluate an if expression
function eval_if(expr, env)
    if eval(if_condition(expr), env)
        return eval(if_consequent(expr), env)
    else
        return eval(if_alternative(expr), env)
    end
end

#########################################################
#Anonymous Function Eval Implementation
#########################################################

# Function to extract arguments from an anonymous function
function anonymous_function_arguments(expr)
    if name(expr.args[1])
        return [expr.args[1]]
    end
    let args = []
        for i in eachindex(expr.args[1].args)
            push!(args, expr.args[1].args[i])
        end
        return args
    end
end

#Function to evaluate an anonymous function
function eval_anonymous(expr, env)
    let args = anonymous_function_arguments(expr), block = function_block(expr)
        let fnc = function_def(args, block, env)
            return fnc
        end
    end
end

#########################################################
#Global Eval Implementation
#########################################################

# Function to find the global environment
function find_global_env(env)
    let cur_env = env
        while !isnothing(cur_env.prev)
            cur_env = cur_env.prev
        end
        return cur_env
    end
end

# Function to extract the variable assignment expression from a global assignment expression
function global_assignment(expr)
    return expr.args[1]
end

# Function to extract the variable being assigned in a global assignment expression
function global_assignment_variable(expr)
    return global_assignment(expr).args[1]
end

# Function to extract the name of a variable from a global assignment expression
function global_assignment_variable_name_handling(expr)
    if name(global_assignment_variable(expr))
        varName = expr.args[1].args[1]
    else
        varName = expr.args[1].args[1].args[1]
    end
    return varName
end

# Function to evaluate a global assignment expression
function eval_global(expr, env)
    let temp = env, env = find_global_env(env), result = eval(global_assignment(expr), temp), varName = global_assignment_variable_name_handling(expr)
        for i in eachindex(temp.data)
            if varName == temp.data[i].name
                pushfirst!(env.data, temp.data[i])
                deleteat!(temp.data, i)
                break
            end
        end
        return result
    end
end

#########################################################
#Let Eval Implementation
#########################################################

# Function to extract the block from a let expression
function let_block(expr)
    return expr.args[2]
end

# Function to extract assignments from a let expression
function let_assignments(expr)
    let assignments = []
        if assignment(expr.args[1])
            return [expr.args[1]]
        else
            for i in eachindex(expr.args[1].args)
                push!(assignments, expr.args[1].args[i])
            end
        end
        return assignments
    end
end

# Function to evaluate a let expression
function eval_let(expr, env)
    let cur_env = create_environment(env), assignments = let_assignments(expr)
        for i in eachindex(assignments)
            eval(assignments[i], cur_env)
        end
        return eval(let_block(expr), cur_env)
    end
end

#########################################################
#Block Eval Implementations
#########################################################

# Function to extract the return value of a block expression
function block_return(expr)
    return expr.args[block_size(expr)]
end

# Function to determine the size of a block expression
function block_size(expr)
    return length(expr.args)
end

# Function to get the expression at the current line within a block
function current_line(expr, i)
    return expr.args[i]
end

# Function to evaluate a block expression
function eval_block(expr, env)
    for i in 1:block_size(expr)-1
        eval(current_line(expr, i), env)
    end
    return eval(block_return(expr), env)
end

#########################################################
#Assignments Eval Implementation
#########################################################

# Function to evaluate a variable assignment expression
function eval_variable_assignment(expr, env)
    let name = variable_name(expr), value = eval(variable_value(expr), env), primitive = false
        if isa(value, Function)
            primitive = true
        end
        let index = find_variable_env_index(name, env)
            if index != -1
                env.data[index] = variable(name, value, primitive)
            else
                pushfirst!(env.data, variable(name, value, primitive))
            end
        end
        return value
    end
end

# Function to update or add a variable from a function definition
function update_or_add_variable_from_function_definition(expr,env)
    let index = find_variable_env_index(name, env), name = function_name(expr)
        if index != -1
            env.data[index] = variable(name, expr.args[2])
        else
            pushfirst!(env.data, variable(name, expr.args[2]))
        end
    end
end

# Function to extract the expression from a let assignment
function get_let_expression(expr)
    return expr.args[2]
end

# Function to evaluate a let assignment expression
function eval_let_assigment(expr, env)
    let name = expr.args[1], index = find_variable_env_index(name, env), value = eval(get_let_expression(expr), env)
        if index != -1
            env.data[index] = variable(name, value)
        else
            pushfirst!(env.data, variable(name, value))
        end
        return value
    end
end

# Function to evaluate a function assignment expression
function eval_function_assignment(expr,env)
    let name, args, types
        if typeof(expr.args[1]) == Symbol
            name = expr.args[1]
            args = []
            types = []
        else
            let head = function_head(expr)
                name = function_name(head)
                args = function_arguments(head)
                types = function_argument_types(head)
            end
        end
        let block = function_block(expr)
            let index = find_variable_env_index(name, env), fnc = function_def(args, block, env, types)
                if index != -1
                    let method_index = methodAlreadyExists(env.data[index].value.methods, types)
                        if method_index != -1
                            env.data[index].value.methods[method_index] = method_def(types, block)
                            fnc = env.data[index].value
                        else
                            push!(env.data[index].value.methods, method_def(types, block))
                            fnc = env.data[index].value
                        end
                    end
                else
                    pushfirst!(env.data, variable(name, fnc))
                end
                return fnc
            end
        end
    end
end

# Function to evaluate an assignment expression
function eval_assignment(expr, env)
    if variable_assignment(expr)
        return eval_variable_assignment(expr, env)
    elseif function_definition(expr)
        update_or_add_variable_from_function_definition(expr, env)
    elseif let_expr(get_let_expression(expr))
        return eval_let_assigment(expr, env)
    else
        return eval_function_assignment(expr, env)
    end
end

# Function to evaluate a name expression
function eval_name(expr, env)
    let index = find_variable_env_index(expr, env)
        while index == -1
            env = env.prev
            if isnothing(env)
                return "Unbound name -- EVAL-NAME"
            end
            index = find_variable_env_index(expr, env)
        end
        return eval(env.data[index].value, env)
    end
end

# Function to evaluate a dollar expression
function eval_dollar(expr, env)
    let name = expr.args[1], index = find_variable_env_index(name, env), backupEnv = env
        while index == -1
            env = env.prev
            if isnothing(env)
                return "Unbound name -- EVAL-NAME"
            end
            index = find_variable_env_index(name, env)
        end
        if (typeof(env.data[index].value) == function_def)
            return eval(env.data[index].value.block, backupEnv)
        else
            return eval(env.data[index].value, backupEnv)
        end
    end
end

#########################################################
#Function Call Eval Implementation
#########################################################

# Function to extract the arguments from an expression
function expression_arguments(expr)
    return expr.args[1]
end

# Function to call a primitive function
function call_primitive(func, args)
    func(args...)
end

# Function to evaluate a primitive function call
function eval_primitive_call(var, call_arguments, env)
    for i in eachindex(call_arguments)
        let arg = eval(call_arguments[i], env)
            if typeof(arg) == Expr && quote_form(arg)
                arg = arg.args[1]
            end
            call_arguments[i] = arg
        end
    end
    return call_primitive(var.value, call_arguments)
end

# Function to evaluate an anonymous function expression call
function eval_anonymous_call(expr, env)
    let anonymous_args = anonymous_function_arguments(expression_arguments(expr)), block = function_block(expression_arguments(expr)), sub_env = create_environment(env)
        let fnc = function_def(anonymous_args, block, sub_env), call_arguments = function_call_arguments(expr)
            for i in eachindex(call_arguments)
                let cur_arg = fnc.args[i], cur_value = eval(call_arguments[i], sub_env)
                    while cur_value == Expr
                        cur_value = eval(cur_value, sub_env)
                    end
                    eval(:($cur_arg = $cur_value), sub_env)
                end
            end
            return eval(fnc.methods[1].block, sub_env)
        end
    end
end

# Function to evaluate a function expression call
function eval_fexpr_call(var, call_arguments, env)
    var = variable(var.name, fexpr_def(var.value.args, var.value.block, var.value.env))
    let fnc = var.value, scope = var.value.env
        if length(fnc.args) != length(call_arguments)
            return "Wrong number of arguments -- EVAL-FUNCTION"
        end
        for i in eachindex(call_arguments)
            let cur_arg = fnc.args[i], cur_value = Expr(:quote, call_arguments[i])
                pushfirst!(scope.data, variable(cur_arg, cur_value))
            end
        end
        let eval_var_index = find_variable_env_index(:eval, scope)
            scope.data[eval_var_index] = variable(:eval, (expr) -> scoped_eval(expr, env), true)
            return eval(fnc.block, scope)
        end
    end
end

# Function to evaluate a macro call
function eval_macro_call(var, call_arguments, env)
    let fnc = var.value
        if length(fnc.args) != length(call_arguments)
            return "Wrong number of arguments -- EVAL-FUNCTION"
        end
        for i in eachindex(call_arguments)
            let cur_arg = fnc.args[i], cur_value = Expr(:quote, call_arguments[i])
                eval(:($cur_arg = $cur_value), env)
            end
        end
        return eval(fnc.block, env)
    end
end

# Function to verify the the call is really a function
function function_call_verifier(fnc, call_arguments)
    if typeof(fnc) != function_def
        return "This variable does not have a function -- EVAL-FUNCTION"
    elseif length(fnc.args) != length(call_arguments)
        return "Wrong number of arguments -- EVAL-FUNCTION"
    end
end

# Function to execute a method
function execute_method(method_index, call_arguments, fnc, scope)
    if method_index == -1
        return "There is no method for the received call types -- EVAL-FUNCTION"
    else
        for i in eachindex(call_arguments)
            let cur_arg = fnc.args[i], cur_value = call_arguments[i]
                eval(:($cur_arg = $cur_value), scope)
            end
        end
        return eval(fnc.methods[method_index].block, scope)
    end
end

#Function to process a function call
function process_function_call(var, call_arguments, env)
    let fnc = var.value, scope = var.value.env, methods = var.value.methods, types = []
        function_call_verifier(fnc, call_arguments)
        for i in eachindex(call_arguments)
            let cur_value = eval(call_arguments[i], env)
                while cur_value == Expr
                    cur_value = eval(cur_value, env)
                end
                push!(types, Symbol(typeof(cur_value)))
                call_arguments[i] = cur_value
            end
        end
        let method_index = methodExists(methods, types)
            return execute_method(method_index, call_arguments, fnc, scope)
        end
    end
end

# Function to evaluate a function call
function eval_function(expr, env)
    let name = function_name(expr), backupEnv = env, index = find_variable_env_index(name, env)
        if anonymous(expr)
            return eval_anonymous_call(expr, env)
        else
            while index == -1
                env = env.prev
                if isnothing(env)
                    return "Unbound function -- EVAL-FUNCTION"
                end
                index = find_variable_env_index(name, env)
            end
            let var = env.data[index], call_arguments = function_call_arguments(expr)
                if primitive(var)
                    return eval_primitive_call(var, call_arguments, backupEnv)
                elseif fexpr_call(var.value)
                    return eval_fexpr_call(var, call_arguments, backupEnv)
                elseif macro_call(var.value)
                    return eval_macro_call(var, call_arguments, backupEnv)
                end
                return process_function_call(var, call_arguments, backupEnv)
            end
        end
    end
end

#########################################################
#Quote Eval Implementation
#########################################################

# Function to extract the arguments of a quote expression
function quote_arguments(expr)
    return expr.args[1]
end

# Function to extract the inner arguments of a quote expression
function quote_inner_arguments(expr)
    return expr.args[1].args
end

# Function to check if an expression is a quoted value.
function quote_get_value_expr(expr)
    return expr.head == :$
end

# Function to retrieve the arguments of an inner expression.
function quote_inner_expression_arguments(expr)
    return expr.args
end

# Function to evaluate a quoted expression in the given environment.
function eval_quote(expr, env)
    if typeof(expr) == QuoteNode
        return expr.value
    end
    let args = quote_arguments(expr)
        if typeof(args) != Expr
            return expr
        end
        if typeof(args) == Expr && quote_get_value_expr(args)
            return eval(quote_inner_arguments(expr)[1], env)
        end
        let outer_args = quote_inner_arguments(expr)
            for inner_expression_index in eachindex(outer_args)
                if typeof(outer_args[inner_expression_index]) == Expr && quote_get_value_expr(outer_args[inner_expression_index])
                    let args_inner = quote_inner_expression_arguments(outer_args[inner_expression_index])
                        outer_args[inner_expression_index] = eval(args_inner[1], env)
                    end
                end
            end
        end
        return expr.args[1]
    end
end

#########################################################
#Eval Implementation
#########################################################

# Function to determine if an expression is self-evaluating
function self_evaluated(expr)
    let type = typeof(expr)
        if type <: Number || type == String || type == LineNumberNode || isnothing(expr) || type == Bool || isa(expr, Function) || type == function_def || type == fexpr_def
            return true
        end
    end
    return false
end

# Function to evaluate a given expression in a given environment
function eval(expr, env)
    if typeof(expr) == Expr
    end
    if self_evaluated(expr)
        return expr
    elseif name(expr)
        return eval_name(expr, env)
    elseif quote_form(expr)
        return eval_quote(expr, env)
    elseif assignment(expr)
        return eval_assignment(expr, env)
    elseif block(expr)
        return eval_block(expr, env)
    elseif let_expr(expr)
        return eval_let(expr, env)
    elseif global_expr(expr)
        return eval_global(expr, env)
    elseif if_expr(expr)
        return eval_if(expr, env)
    elseif and(expr)
        return eval_and(expr, env)
    elseif or(expr)
        return eval_or(expr, env)
    elseif ifAnonymous(expr)
        return eval_anonymous(expr, env)
    elseif function_call(expr)
        return eval_function(expr, env)
    elseif fexpr(expr)
        return eval_fexpr(expr, env)
    elseif ismacro(expr)
        return eval_macro(expr, env)
    elseif dollar(expr)
        return eval_dollar(expr, env)
    end
    return
end

#########################################################
#REPL Implementation And MetaJulia Eval Implementation
#########################################################

# Function to initialize the initial bindings in an environment
function initial_bindings()
    let env = environment([], nothing), primitives = [+, -, *, /, >, >=, <=, <, !, ==, !=, ===, !==, println, gensym, methods]
        for i in primitives
            push!(env.data, make_primitive(i))
        end
        return env
    end
end

# Define the initial environment for the metajulia eval
global metajulia_eval_env = initial_bindings()

#Function to evaluate an expression in the metajulia environment
function metajulia_eval(expr)
    return eval(expr, metajulia_eval_env)
end

# Function to read, input, parse, and loop by evaluating repeatedly the provided environment.
function metajulia_repl(env)
    print(">> ")
    let input = readline()
        let expr = Meta.parse(input, raise=false)
            while typeof(expr) == Expr && expr.head == Symbol("incomplete")
                input = input * "\n" * readline()
                expr = Meta.parse(input, raise=false)
            end
            let output = eval(expr, env)
                println(output)
            end
        end
    end
    metajulia_repl(env)
end

#To use metajulia_eval and consequently the tests this line needs to be commented. 
#To use the reple simply uncomment this line.
# metajulia_repl(initial_bindings())