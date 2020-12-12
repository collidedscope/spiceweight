require "big"
require "json"
require "option_parser"

class Spiceweight
  Ops = {
    "  " => :push, " \t " => :copy, " \t\n" => :slide, "\n  " => :label,
    "\n \t" => :call, "\n \n" => :jump, "\n\t " => :jz, "\n\t\t" => :jn,
    " \n\n" => :pop, " \n " => :dup, " \n\t" => :swap, "\t   " => :add,
    "\t  \t" => :sub, "\t  \n" => :mul, "\t \t " => :div, "\t \t\t" => :mod,
    "\t\t " => :store, "\t\t\t" => :load, "\n\t\n" => :ret, "\t\n\t " => :ichr,
    "\t\n\t\t" => :inum, "\t\n  " => :ochr, "\t\n \t" => :onum, "\n\n\n" => :exit,
  }

  alias Num = Int64 | BigInt
  alias Insn = {Symbol, Int64}

  @tokens : Array(Char)

  def initialize(src)
    @tokens = src.delete("^ \t\n").chars
    @insns = [] of Insn
  end

  def parse
    buffer = ""
    line = char = 1

    while t = @tokens.shift?
      buffer += t
      if Ops.none? { |k, v| k.starts_with? buffer }
        abort "illegal token sequence #{buffer.inspect} " +
          "(line #{line}, char #{char})"
      end

      if op = Ops[buffer]?
        if Ops.key_index(buffer).not_nil! < 8
          arg = parse_number
          line += 1
        end
        @insns << {op, arg || 0i64}
        buffer = ""
      end
      if t == '\n'
        line += 1
        char = 1
      else
        char += 1
      end
    end
  end

  macro binop(op)
    tmp = stack.pop
    @stack[-1] =
      begin
        @stack[-1] {{op.id}} tmp
      rescue OverflowError
        BigInt.new(@stack[-1]) {{op.id}} tmp
      end
  end

  @stack = [] of Num
  @heap = {} of Num => Num
  @count = 0
  @benchmarks = {} of Int64 => Array(Time::Span)
  getter :stack, :heap, :count, :benchmarks

  def interpret(bench, bench_labels, io = STDOUT)
    jumps = {} of Int64 => Int32
    @insns.each_with_index do |(op, arg), i|
      jumps[arg] = i if op == :label
    end

    ip = -1i32
    calls = [] of {Int64, Int32}
    call_times = {} of Int64 => Time

    while insn = @insns[ip += 1]?
      now = Time.local if bench # ASAP for greatest accuracy
      op, arg = insn
      @count += 1 unless op == :label

      case op
      # stack
      when :push ; @stack << arg
      when :pop  ; @stack.pop
      when :dup  ; @stack << @stack[-1]
      when :swap ; @stack[-1], @stack[-2] = @stack[-2], @stack[-1]
      when :copy ; @stack << @stack[-1 - arg]
      when :slide; @stack[-arg - 1, arg] = [] of Int64
        # math
      when :add; binop :+
      when :sub; binop :-
      when :mul; binop :*
      when :div; tmp = @stack.pop; @stack[-1] //= tmp
      when :mod; a, b = @stack.pop 2
      @stack << case {a, b}
      when {Int64, Int64}
        a % b
      else
        BigInt.new(a) % b
      end
      # flow
      when :jump; ip = jumps[arg]
      when :jz  ; ip = jumps[arg] if @stack.pop == 0
      when :jn  ; ip = jumps[arg] if @stack.pop < 0
      when :call
        calls << {arg, ip}
        ip = jumps[arg]
        if bench && bench_labels.includes? arg
          call_times[arg] = now.not_nil!
        end
      when :ret
        arg, ip = calls.pop
        if bench && bench_labels.includes? arg
          (@benchmarks[arg] ||= [] of Time::Span) <<
            now.not_nil! - call_times[arg]
        end
      when :exit; break
        # heap
      when :store; k, v = @stack.pop 2; @heap[k] = v
      when :load ; @stack << @heap.fetch @stack.pop.to_i, 0i64
        # IO
      when :ichr; @heap[@stack.pop] = Int64.new (b = STDIN.read_byte) ? b : -1
      when :inum; @heap[@stack.pop] = Int64.new STDIN.peek.empty? ? 0 : gets.not_nil!
      when :ochr; io << @stack.pop.to_i.chr
      when :onum; io << @stack.pop
      end
    end
  end

  def parse_number
    if i = @tokens.index '\n'
      num = @tokens.shift i + 1
      num[0] = '-' if num[0] == '\t'
      num.join.tr(" \t", "01").to_i64 2
    end
  end
end

report = false
bench = false

op = OptionParser.parse do |parser|
  parser.banner = <<-EOS
Spiceweight is a Whitespace interpreter with some cool features:
    * negative heap addressing so that stdlib subroutines never clobber user data
    * arbitrary precision integers (but only when necessary, so it's)
    * pretty fast!

Usage: #{PROGRAM_NAME} [OPTIONS] FILE
EOS

  parser.on("-r", "--report",
    "Display stack, heap, and instruction count after executing") {
    report = true
  }
  parser.on("-b", "--bench", <<-EOS) {
Benchmark calls to labels whose names begin with 'bench'
Labels in Whitespace code are purely numerical, of course, so this relies on
some other tool having previously generated a symbol table mapping label names
to their corresponding numbers. The Spitewaste assembler is one such tool.
EOS
    bench = true
  }
  parser.on("-h", "--help", "Show this help") {
    puts parser
    exit
  }

  parser.invalid_option do |flag|
    STDERR.puts "ERROR: #{flag} is not a valid option."
    abort parser
  end
end

abort op unless src = ARGV[0]?
sw = Spiceweight.new File.read src
sw.parse

if bench
  cache_dir = File.expand_path "~/.cache/spitewaste", home: Path.home
  base = File.basename File.expand_path(ARGV[0]).tr("/", "-")[1..], ".ws"
  symbol_file = File.join cache_dir, base + ".json"
  unless File.exists?(symbol_file)
    raise "Can't benchmark without symbol file"
  end

  symbol_table = File.open(symbol_file) { |f| JSON.parse f }.as_h
  bench_labels = symbol_table.select { |k, v| k.starts_with? "bench" }

  sw.interpret true, bench_labels.values.to_set
  sw.benchmarks.each do |label, times|
    n = times.size
    puts "#{bench_labels.key_for label} called #{n} times",
      "    avg: #{times.sum / n}",
      "    min: #{times.min}",
      "    max: #{times.max}"
  end
else
  sw.interpret nil, [] of Nil
end

STDERR.puts sw.stack, sw.heap, sw.count if report
exit sw.stack.size # punish dirty exit stack
