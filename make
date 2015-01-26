#!/usr/bin/env ruby

require 'colorize'
require 'fileutils'

case ARGV.size
when 1
  main_module = ARGV.first
when 2
  runtime = ARGV[0]
  app = ARGV[1]
  main_module = "#{app}_#{runtime}"
else
  raise "Please specify argument."
end

unless File.exists? "#{main_module}.erl"
  raise "Modoule not found."
end

def compile(erl)
  puts "Compile(#{erl})".blue
  command = "erlc -DRING=1000 #{erl}"
  status = system(command)
  if !status
    puts "Failure(#{command})!".red
    exit!
  end
end


if Dir.glob("*.hrl").map{|hrl| File::Stat.new(hrl).mtime}.max >
   Dir.glob("*.beam").map{|erl| File::Stat.new(erl).mtime}.min
  # Compile all if header is modifiled
  Dir.glob("*.erl").each do |erl|
    compile(erl)
  end
else
  # Compile all modified erl
  Dir.glob("*.erl").each do |erl|
    base = erl[/(.+)\.erl/,1]
    beam = "#{base}.beam"
    if not File.exists? beam
      compile(erl)
    else
      erl_s, beam_s = File::Stat.new(erl), File::Stat.new(beam)
      if erl_s.mtime > beam_s.ctime
        compile(erl)
      else
        next
      end
    end
  end
end

exec "erl -run #{main_module} main a"
