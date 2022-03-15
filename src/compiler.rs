pub(self) mod lua {
    use std::{
        collections::HashMap,
        fmt::{self, write},
        hash::Hash,
        mem, ops,
    };

    use crate::parser::lua::*;
    pub use crate::parser::lua::*;
    use anyhow::{bail, Error, Result};
    use nom::Offset;

    #[derive(Debug, PartialEq, Clone, Copy)]
    enum Opcode {
        Push,
        Pop,
        Add,
        Subtract,
        Error,
        Ret,
        Halt,
        Jmp,
        Call,
        Load,
        Store,
        PushE,
        Jgr,
        Jmpr,
    }

    impl Default for Opcode {
        fn default() -> Self {
            Opcode::Halt
        }
    }

    impl fmt::Display for Opcode {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self {
                Opcode::Push => write!(f, "push"),
                Opcode::PushE => write!(f, "pushe"),
                Opcode::Pop => write!(f, "pop"),
                Opcode::Add => write!(f, "add"),
                Opcode::Subtract => write!(f, "subtract"),
                Opcode::Ret => write!(f, "ret"),
                Opcode::Halt => write!(f, "halt"),
                Opcode::Jmp => write!(f, "jmp"),
                Opcode::Call => write!(f, "call"),
                Opcode::Load => write!(f, "load"),
                Opcode::Store => write!(f, "store"),
                Opcode::Jgr => write!(f, "jgr"),
                Opcode::Jmpr => write!(f, "jmpr"),
                _ => write!(f, "error"),
            }
        }
    }

    #[derive(Debug, Clone, PartialEq)]
    enum Operand {
        Offset(i32),
        Number(Number),
        String(String),
        Ident(Ident),
        Error,
    }

    impl Operand {
        fn to_offset(&self) -> Result<i32> {
            match self {
                Operand::Offset(o) => Ok(*o),
                _ => bail!("operand is not offset"),
            }
        }
        fn to_int(&self) -> Result<i32> {
            match self {
                Operand::Number(n) => match n {
                    Number::Integer(i) => Ok(*i),
                    _ => bail!("operand is not integer"),
                },
                _ => bail!("operand is not number"),
            }
        }
    }

    impl ops::Add<Operand> for Operand {
        type Output = Operand;
        fn add(self, _rhs: Operand) -> Operand {
            if mem::discriminant(&self) != mem::discriminant(&_rhs) {
                Operand::Error
            } else {
                match self {
                    Operand::Number(lhs) => {
                        if let Operand::Number(rhs) = _rhs {
                            Operand::Number(lhs + rhs)
                        } else {
                            Operand::Error
                        }
                    }
                    _ => Operand::Error,
                }
            }
        }
    }

    impl ops::Sub<Operand> for Operand {
        type Output = Operand;
        fn sub(self, _rhs: Operand) -> Operand {
            if mem::discriminant(&self) != mem::discriminant(&_rhs) {
                Operand::Error
            } else {
                match self {
                    Operand::Number(lhs) => {
                        if let Operand::Number(rhs) = _rhs {
                            Operand::Number(lhs - rhs)
                        } else {
                            Operand::Error
                        }
                    }
                    _ => Operand::Error,
                }
            }
        }
    }

    impl ops::Add<Number> for Number {
        type Output = Number;
        fn add(self, _rhs: Number) -> Number {
            match self {
                Number::Integer(lhs) => match _rhs {
                    Number::Integer(rhs) => Number::Integer(lhs + rhs),
                    Number::Float(rhs) => Number::Float(lhs as f32 + rhs),
                    _ => Number::Error,
                },
                Number::Float(lhs) => match _rhs {
                    Number::Integer(rhs) => Number::Float(lhs + rhs as f32),
                    Number::Float(rhs) => Number::Float(lhs + rhs),
                    _ => Number::Error,
                },
                _ => Number::Error,
            }
        }
    }

    impl ops::Sub<Number> for Number {
        type Output = Number;
        fn sub(self, _rhs: Number) -> Number {
            match self {
                Number::Integer(lhs) => match _rhs {
                    Number::Integer(rhs) => Number::Integer(lhs - rhs),
                    Number::Float(rhs) => Number::Float(lhs as f32 - rhs),
                    _ => Number::Error,
                },
                Number::Float(lhs) => match _rhs {
                    Number::Integer(rhs) => Number::Float(lhs - rhs as f32),
                    Number::Float(rhs) => Number::Float(lhs - rhs),
                    _ => Number::Error,
                },
                _ => Number::Error,
            }
        }
    }

    impl From<&Exp> for Operand {
        fn from(exp: &Exp) -> Self {
            match exp {
                Exp::Ident(i) => Operand::Ident(i.clone()),
                Exp::String(i) => Operand::String(i.clone()),
                Exp::Number(i) => Operand::Number(*i),
                _ => Operand::Error,
            }
        }
    }

    #[derive(Debug, Clone, Default)]
    struct Command {
        instruction: Opcode,
        operands: Option<Vec<Operand>>,
    }

    impl fmt::Display for Operand {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self {
                Operand::Number(n) => write!(f, "{}", n),
                Operand::String(s) => write!(f, "{}", s),
                Operand::Ident(id) => write!(f, "{}", id.name),
                Operand::Offset(o) => write!(f, "{}", o),
                _ => write!(f, "error"),
            }
        }
    }

    impl fmt::Display for Command {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "{}", self.instruction);
            if self.operands.is_some() {
                for i in self.operands.as_ref().unwrap() {
                    write!(f, " {}", i);
                }
            }
            Ok(())
        }
    }

    #[derive(Debug, Default)]
    struct Program {
        commands: Vec<Command>,
        stack: Vec<Operand>,
    }

    fn disassemble(commands: &[Command]) -> (String, Vec<String>) {
        let mut res = String::from("");
        let mut vres = Vec::new();
        for cmd in commands {
            res += &cmd.to_string();
            res.push('\n');
            vres.push(cmd.to_string().clone());
        }
        (res, vres)
    }

    #[derive(Debug)]
    struct VM {
        pc: usize,
        bp: usize,
        prog: Program,
    }

    impl VM {
        fn new() -> VM {
            VM {
                pc: 0,
                bp: 0,
                prog: Program::default(),
            }
        }

        fn reset(&mut self) {
            self.pc = 0;
            self.prog.stack.clear();
        }

        fn set_commands(&mut self, cmds: &[Command]) {
            self.prog.commands = cmds.to_owned();
        }

        fn run(&mut self) {
            while self.pc < self.prog.commands.len() {
                let cmd = &self.prog.commands[self.pc];
                self.pc += 1;
                //println!("{}", cmd);
                match cmd.instruction {
                    Opcode::Push => {
                        self.prog
                            .stack
                            .push(cmd.operands.as_ref().unwrap().last().unwrap().clone());
                    }
                    Opcode::PushE => {
                        self.prog.stack.push(Operand::Error);
                    }
                    Opcode::Halt => {
                        return;
                    }
                    Opcode::Load => {
                        self.prog.stack.push(
                            self.prog.stack[(self.bp as i32
                                + cmd
                                    .operands
                                    .as_ref()
                                    .unwrap()
                                    .last()
                                    .unwrap()
                                    .to_offset()
                                    .unwrap())
                                as usize]
                                .clone(),
                        );
                    }
                    Opcode::Add => {
                        let rhs = self.prog.stack.pop().unwrap();
                        let lhs = self.prog.stack.pop().unwrap();
                        let res = lhs + rhs;
                        self.prog.stack.push(res);
                    }
                    Opcode::Subtract => {
                        let rhs = self.prog.stack.pop().unwrap();
                        let lhs = self.prog.stack.pop().unwrap();
                        let res = lhs - rhs;
                        self.prog.stack.push(res);
                    }
                    Opcode::Jmp => {
                        //// fuck this: can't wrap it as a function
                        //// because Rust forbid borrowing as mutable
                        //// as it has been borrow as immutable simply
                        //// because I don't want to copy a command out of the vector
                        self.pc = cmd.operands.as_ref().unwrap()[0].to_offset().unwrap() as usize;
                    }
                    Opcode::Jgr => {
                        let cond = self.prog.stack.pop().unwrap().to_int().unwrap();
                        //dbg!(cond);
                        if cond < 0 {
                            self.pc +=
                                cmd.operands.as_ref().unwrap()[0].to_offset().unwrap() as usize - 1;
                        }
                    }
                    Opcode::Jmpr => {
                        self.pc +=
                            cmd.operands.as_ref().unwrap()[0].to_offset().unwrap() as usize - 1;
                    }
                    Opcode::Ret => {
                        let res = self.prog.stack.pop().unwrap();
                        // what a mess
                        self.prog.stack.resize(self.bp+1, Operand::Error);
                        self.bp = self.prog.stack.pop().unwrap().to_offset().unwrap() as usize;
                        self.pc = self.prog.stack.pop().unwrap().to_offset().unwrap() as usize;
                        let argn = self.prog.stack.pop().unwrap().to_int().unwrap() as usize;
                        for _ in (0..argn) {
                            self.prog.stack.pop();
                        }
                        self.prog.stack.push(res);
                    }
                    Opcode::Call => {
                        self.prog.stack.push(Operand::Offset(self.pc as i32));
                        self.prog.stack.push(Operand::Offset(self.bp as i32));
                        self.bp = self.prog.stack.len() - 1;
                        self.pc = cmd.operands.as_ref().unwrap()[0].to_offset().unwrap() as usize;
                    }
                    Opcode::Store => {
                        let offset = cmd.operands.as_ref().unwrap()[0].to_offset().unwrap();
                        let res = self.prog.stack.last().unwrap();
                        self.prog.stack[self.bp + offset as usize] = res.clone();
                    }
                    _ => {}
                }
            }
        }

        fn top(&self) -> Option<&Operand> {
            self.prog.stack.last()
        }
    }

    impl Default for VM {
        fn default() -> Self {
            Self::new()
        }
    }

    #[derive(Debug, Eq, PartialEq, Hash)]
    enum SymType {
        Func,
        LocalVar,
        GlobalVar,
    }

    #[derive(Debug, Eq, PartialEq, Hash)]
    struct Sym {
        pub name: String,
        pub sym_type: SymType,
    }

    #[derive(Debug, Clone)]
    enum Digest {
        FuncSign(FuncSign),
        Error,
    }

    #[derive(Debug, Clone)]
    struct FuncSign {
        params: Vec<String>,
    }

    #[derive(Debug, Clone)]
    struct SymInfo {
        i_offset: i32,
        digest: Digest,
    }

    #[derive(Debug, Default)]
    struct Compiler {
        symtable: HashMap<Sym, SymInfo>,
        commands: Vec<Command>,
        pc: i32,
    }

    impl Compiler {
        fn new() -> Compiler {
            Compiler {
                commands: {
                    let mut cmd = vec![Command {
                        instruction: Opcode::Jmp,
                        operands: None,
                    }];
                    cmd
                },
                ..Default::default()
            }
        }

        fn check_symtable(&self, oprd: Operand) -> Command {
            match oprd {
                Operand::Ident(id) => {
                    let si = &self.symtable[&Sym {
                        name: id.name,
                        sym_type: SymType::LocalVar,
                    }];
                    Command {
                        instruction: Opcode::Load,
                        operands: Some(vec![Operand::Offset(si.i_offset)]),
                    }
                }
                _ => Command {
                    instruction: Opcode::Push,
                    operands: Some(vec![oprd]),
                },
            }
        }

        fn compile_binop(&mut self, binop: &Binop) -> Result<Vec<Command>> {
            let mut res: Vec<Command> = Vec::new();
            let exp_iter = &mut binop.exps.iter().peekable();
            let ops_iter = &mut binop.ops.iter();
            let e = exp_iter.next().unwrap();
            res.push(self.check_symtable(Operand::from(e)));

            while exp_iter.peek().is_some() {
                let e = exp_iter.next().unwrap();
                let op = ops_iter.next().unwrap();
                res.push(self.check_symtable(Operand::from(e)));
                res.push(Command {
                    instruction: match op {
                        Op::Plus => Opcode::Add,
                        Op::Subtract => Opcode::Subtract,
                        Op::LessThan => Opcode::Subtract,
                        _ => Opcode::Error,
                    },
                    operands: None,
                });
            }

            Ok(res)
        }

        fn compile_ident(&mut self, id: &Ident) -> Result<Vec<Command>> {
            Ok(vec![self.check_symtable(Operand::Ident(id.clone()))])
        }

        fn compile_number(&mut self, n: &Number) -> Result<Vec<Command>> {
            Ok(vec![Command {
                instruction: Opcode::Push,
                operands: Some(vec![Operand::Number(*n)]),
            }])
        }

        fn compile_exp(&mut self, exp: &Exp) -> Result<Vec<Command>> {
            match exp {
                Exp::Binop(binop) => self.compile_binop(binop),
                Exp::Ident(id) => self.compile_ident(id),
                Exp::Number(n) => self.compile_number(n),
                Exp::FuncCall(call) => compile_funccall(self, call),
                _ => bail!("Fail to compile exp"),
            }
        }

        fn compile_cond(&mut self, cond: &Cond) -> Result<Vec<Command>> {
            let mut exps = Vec::new();
            let mut blks = Vec::new();
            let mut e_tt = 0;
            let mut b_tt = 0;

            for (exp, block) in &cond.pairs {
                let r = self.compile_exp(exp).unwrap();
                e_tt += r.len();
                exps.push(r);
                let r = self.compile_block(block).unwrap();
                b_tt += r.len();
                blks.push(r);
            }

            e_tt += exps.len();
            b_tt += blks.len();
            let end_of_exps = Command {
                instruction: Opcode::Jmpr,
                operands: Some(vec![Operand::Offset(b_tt as i32 + 1)]),
            };

            let mut blk_depth = 0;

            for i in (0..exps.len()) {
                // current span = plus one jmp inst.
                let n = exps[i].len() + 1;
                dbg!(n);
                e_tt -= n;
                // plus one final jump to exit the condition block
                let offset = e_tt + blk_depth + 1 + 1;

                exps[i].push(Command {
                    instruction: Opcode::Jgr,
                    operands: Some(vec![Operand::Offset(offset as i32)]),
                });

                let n = blks[i].len() + 1;
                b_tt -= n;
                let offset = b_tt + 1;

                blks[i].push(Command {
                    instruction: Opcode::Jgr,
                    operands: Some(vec![Operand::Offset(offset as i32)]),
                });

                blk_depth += n;
            }
            let mut res = Vec::new();
            for mut e in exps {
                res.append(&mut e);
            }
            res.push(end_of_exps);
            for mut b in blks {
                res.append(&mut b);
            }

            Ok(res)
        }

        fn compile_stat(&mut self, stat: &Stat) -> Result<Vec<Command>> {
            match stat {
                Stat::Local(local) => self.compile_local(local),
                Stat::Assign(assign) => self.compile_assign(assign),
                Stat::FuncDef(def) => self.compile_funcdef(def),
                Stat::FuncCall(call) => compile_funccall(self, call),
                Stat::Cond(cond) => self.compile_cond(cond),
                _ => bail!("Fail to compile stat"),
            }
        }

        fn compile_laststat(&mut self, last: &LastStat) -> Result<Vec<Command>> {
            let mut res: Vec<Command> = Vec::new();
            match last {
                LastStat::Return(ret) => {
                    for e in &ret.exps {
                        res.append(&mut self.compile_exp(e).unwrap());
                    }
                    res.push(Command {
                        instruction: Opcode::Ret,
                        operands: None,
                    });
                    Ok(res)
                }
                _ => bail!("Fail to compile laststat"),
            }
        }

        fn compile_block(&mut self, block: &Block) -> Result<Vec<Command>> {
            let mut res: Vec<Command> = Vec::new();
            match block {
                Block::Chunk(chunk) => {
                    res.append(&mut self.compile_chunk(chunk).unwrap());
                    Ok(res)
                }
                _ => bail!("Fail to compile block"),
            }
        }

        fn compile_funcdef(&mut self, funcdef: &FuncDef) -> Result<Vec<Command>> {
            self.symtable.insert(
                Sym {
                    name: funcdef.name.clone(),
                    sym_type: SymType::Func,
                },
                SymInfo {
                    i_offset: self.commands.len() as i32,
                    digest: Digest::FuncSign(FuncSign {
                        params: funcdef.body.params.to_vec(),
                    }),
                },
            );
            let mut param_code = Vec::new();

            let mut argc = 1;
            for arg in &funcdef.body.params {
                let offset = -3 - argc + 1;
                param_code.push(Command {
                    instruction: Opcode::Load,
                    operands: Some(vec![Operand::Offset(offset)]),
                });
                self.pc = offset;
                self.symtable.insert(
                    Sym {
                        name: arg.clone(),
                        sym_type: SymType::LocalVar,
                    },
                    SymInfo {
                        i_offset: argc,
                        digest: Digest::Error,
                    },
                );

                argc += 1;
            }

            let locals = scan_locals(&funcdef.body.block).unwrap();
            for local in &locals {
                self.symtable.insert(
                    Sym {
                        name: local.clone(),
                        sym_type: SymType::LocalVar,
                    },
                    SymInfo {
                        i_offset: argc,
                        digest: Digest::Error,
                    },
                );
                param_code.push(Command {
                    instruction: Opcode::PushE,
                    operands: None,
                });
                argc += 1;
            }

            let mut block = self.compile_block(&funcdef.body.block).unwrap();
            self.commands.append(&mut param_code);
            self.commands.append(&mut block);

            let alllocals: Vec<&String> = funcdef.body.params.iter().chain(locals.iter()).collect();
            for arg in alllocals {
                self.symtable.remove(&Sym {
                    name: arg.clone(),
                    sym_type: SymType::LocalVar,
                });
            }

            Ok(Vec::new())
        }

        // there are differences between assignment and local variable declaration,
        // for example, a, b = 1, 2 is a possible assignment,
        // but declaration does not allow multiple variables on the left side
        // (I mean, for this language)
        fn compile_assign(&mut self, assign: &Assign) -> Result<Vec<Command>> {
            let mut res: Vec<Command> = Vec::new();
            let lhs = &assign.lhs;
            let rhs = &assign.rhs[0];

            res.append(&mut self.compile_exp(rhs).unwrap());

            let storage = &self.symtable[&Sym {
                name: lhs.name.clone(),
                sym_type: SymType::LocalVar,
            }];
            res.push(Command {
                instruction: Opcode::Store,
                operands: Some(vec![Operand::Offset(storage.i_offset)]),
            });
            Ok(res)
        }

        fn compile_local(&mut self, local: &Local) -> Result<Vec<Command>> {
            let mut res: Vec<Command> = Vec::new();
            // for the moment we just don't think about arrays or tuples,
            // we assume that the rhs of a local stat is a single expression.
            // we will fix that later.
            let lhs = &local.lhs;
            let rhs = &local.rhs[0];

            res.append(&mut self.compile_exp(rhs).unwrap());

            let storage = &self.symtable[&Sym {
                name: lhs.name.clone(),
                sym_type: SymType::LocalVar,
            }];
            res.push(Command {
                instruction: Opcode::Store,
                operands: Some(vec![Operand::Offset(storage.i_offset)]),
            });
            Ok(res)
        }

        fn compile_chunk(&mut self, chunk: &Chunk) -> Result<Vec<Command>> {
            let mut res: Vec<Command> = Vec::new();
            //dbg!(&chunk);
            for stat in &chunk.stats {
                let mut cmds = self.compile_stat(stat).unwrap();
                res.append(&mut cmds);
            }
            if chunk.laststat.is_some() {
                res.append(
                    &mut self
                        .compile_laststat(chunk.laststat.as_ref().unwrap())
                        .unwrap(),
                );
            }

            Ok(res)
        }

        fn compile(&mut self, lua: &Lua) -> Result<Vec<Command>> {
            match lua {
                Lua::Chunk(chunk) => {
                    let mut chunk_cmd = self.compile_chunk(chunk).unwrap();
                    // got a bad feeling about the copy
                    let mut res = self.commands.to_vec();
                    // update the first command to skip funciton definitions
                    res[0].operands = Some(vec![Operand::Offset(res.len() as i32)]);
                    res.append(&mut chunk_cmd);
                    res.push(Command {
                        instruction: Opcode::Halt,
                        operands: None,
                    });
                    Ok(res)
                }
                _ => bail!("Fail to compile Lua"),
            }
        }
    }

    fn scan_locals(block: &Block) -> Result<Vec<String>> {
        match block {
            Block::Chunk(chunk) => {
                let mut res = Vec::new();
                for stat in &chunk.stats {
                    if let Stat::Local(local) = stat {
                        res.push(local.lhs.name.clone());
                    }
                }
                Ok(res)
            }
            _ => bail!("Expect block"),
        }
    }

    fn compile_funccall(compiler: &mut Compiler, call: &FuncCall) -> Result<Vec<Command>> {
        let si = compiler.symtable[&Sym {
            name: call.name.to_owned(),
            sym_type: SymType::Func,
        }]
            .clone();
        let func_entry = si.i_offset;

        let mut res = Vec::new();
        let offset = 2;
        let mut argn = 0;
        match &si.digest {
            Digest::FuncSign(fs) => {
                argn = fs.params.len();
                for (i, val) in fs.params.iter().rev().zip(call.args.iter().rev()) {
                    let mut exp_result = compiler.compile_exp(val).unwrap();
                    //dbg!(&exp_result);
                    res.append(&mut exp_result);
                }
            }
            _ => bail!("Expected function parameter list"),
        }
        res.push(Command {
            instruction: Opcode::Push,
            operands: Some(vec![Operand::Number(Number::Integer(argn as i32))]),
        });
        res.push(Command {
            instruction: Opcode::Call,
            operands: Some(vec![Operand::Offset(func_entry)]),
        });
        Ok(res)
    }

    fn eval(cmds: &[Command]) -> Operand {
        let mut vm = VM::new();
        vm.set_commands(cmds);
        vm.run();
        vm.top().unwrap().clone()
    }

    #[cfg(test)]
    mod tests {
        use super::*;
        #[test]
        fn test_modules() {
            let empty = vec![];
            let lua = &parse(
                r#"
                function fib(n)
               if n < 2 then
                  return n;
               end
            
               local n1 = fib(n-1);
               local n2 = fib(n-2);

               return n1 + n2;
            end
            local n = fib(30);
            print(n);
            n = 2;
            n = fib(30);
            print(fib(30));

        "#,
            );
            //dbg!(&lua.0);
            assert_eq!(&lua.1[..], &empty);
        }

        #[test]
        fn test_binop() {
            let arithmetic = test_parse("1+2+3-4-5", exp);
            let mut compiler = Compiler::new();
            let res = compiler.compile_exp(&arithmetic.0);
            let asm = disassemble(res.as_ref().unwrap());
            assert_eq!(
                asm.0,
                r#"push 1
push 2
add
push 3
add
push 4
subtract
push 5
subtract
"#
            );
        }

        #[test]
        fn test_simple_func() {
            let arithmetic = parse(
                r#"
                function f(a, b, c)
                    local d = a + b + c;
                    return d;
                end
                f(1, 2, 3);
            "#,
            );
            let mut compiler = Compiler::new();
            //dbg!(&arithmetic.0);
            let res = compiler.compile(&arithmetic.0);
            let asm = disassemble(res.as_ref().unwrap());
            assert_eq!(
                asm.0,
                r#"jmp 13
load -3
load -4
load -5
pushe
load 1
load 2
add
load 3
add
store 4
load 4
ret
push 3
push 2
push 1
push 3
call 1
halt
"#
            );
            assert_eq!(
                eval(res.as_ref().unwrap()),
                Operand::Number(Number::Integer(6))
            );
        }

        fn fib(n: i32) -> i32 {
            if n < 2 {
                return n;
            }
            fib(n - 1) + fib(n - 2)
        }

        #[test]
        fn test_fib() {
            let arithmetic = parse(
                r#"
                function fib(n)
                    if n < 2 then
                        return n;
                    end
                    local n1 = fib(n-1);
                    local n2 = fib(n-2);
                    return n1+n2;
                end
                fib(10);
            "#,
            );
            let mut compiler = Compiler::new();
            let res = compiler.compile(&arithmetic.0);
            let asm = disassemble(res.as_ref().unwrap());
            for i in asm.1 {
                println!("{}", i);
            }
            assert_eq!(
                eval(res.as_ref().unwrap()),
                Operand::Number(Number::Integer(fib(10)))
            );
        }
    }
}
