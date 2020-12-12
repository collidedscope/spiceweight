require "spec"

FIXTURES = File.expand_path "fixtures", __DIR__

def get_fixture(file)
  File.join FIXTURES, file
end

ws = get_fixture "whitespace.ws"
bf = get_fixture "brainfuck.ws"
hw = get_fixture "hello_world.bf"

# If this works, our impementation is almost certainly correct. XD
it "correctly executes a Whitespace interpreter written in Whitespace on a brainfuck interpreter written in Whitespace on a brainfuck program that prints 'Hello World'" do
  output = `echo -n '|' | cat #{bf} - #{hw} | spwt #{ws}`
  output.should eq "Hello World!\n"
end
