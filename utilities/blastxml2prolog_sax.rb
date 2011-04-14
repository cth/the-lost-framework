require 'rubygems'
require 'nokogiri'

include Nokogiri

class String
  def to_prolog
    if self.is_atom?
      self
    else
      "'" + self.gsub("'",'\\\\\'') + "'"
    end
  end

  def to_prolog_array
    arr = []
    self.downcase.each_byte { |b| arr << b.chr }
    arr.to_prolog
  end

  def to_prolog_atom
    self.downcase
  end

  def is_atom?
    self =~ /^[a-z0-9]+(\_|[0-9]|[a-zA-Z])*$/
  end
end

class Array
  def to_prolog
    to_prolog_list
  end

  def to_prolog_facts
    (self.map{ |e| e.to_prolog}).join('\n')
  end

  def to_prolog_list
    "[" + (self.map { |e| e.to_prolog}).join(',') + "]"
  end
end

class FixNum 
  def to_prolog
    self.to_s
  end
end

class HSP
  attr_accessor :features
  
  def initialize(query_id)
    @query_id = query_id
    if query_id =~ /(.+) (\d+) (\d+) (\+|\-)(\d)/
      @seq_id = $1.to_s
      @left_pos = $2.to_i
      @right_pos = $3.to_i
      @strand = $4.to_s
      @frame = $5.to_i
    else
      throw "Could not match query id: #{query_id}"
    end
    @features = {}
  end

  def escore
    @features['evalue'].to_f
  end

  def init_from_xml_node(xml_node)
    throw "unequal size" unless  xml_node['Hsp_qseq'][0].to_s.size ==  xml_node['Hsp_hseq'][0].to_s.size

    @features = 
    {
      :evalue => xml_node['Hsp_evalue'][0].to_s.to_f,
      :qseq => xml_node['Hsp_qseq'][0].to_s.to_prolog_array,
      :hseq => xml_node['Hsp_hseq'][0].to_s.to_prolog_array,
      :midline => xml_node['Hsp_midline'][0].to_s.to_prolog_array
    }

    @features['real_match'] = "yes"
  end
  
  def to_prolog
    prolog_facts = []
    @features.each do |key,val|
      prolog_facts << "#{key.to_s}(#{val})"
    end
    prolog_facts_list = "[" + prolog_facts.join(',') + "]"
    "blast_match(#{@query_id.to_prolog}, #{@left_pos}, #{@right_pos}, #{@strand.to_prolog}, #{@frame}, #{prolog_facts_list})." << "\n"
  end
end

class FakeHSP < HSP
  def initialize(q_id)
    super(q_id)
    @features['real_match'] = "no"
    @features[:hseq] = []
    @features[:hseq] = ("0" * ((@left_pos - @right_pos).abs)).to_prolog_array
  end
end



class PostCallbacks < XML::SAX::Document
  @context = []
  
  def start_element(element, attributes)
    @context = [] if @contect.nil?

    case element
    when "Iteration"
      @context << "Iteration"
    when "Iteration_message"
        @context << element
    when "Iteration_query-def"
        @context << element
    when "Hsp"
      @context << element      
    when "Hsp_evalue"
        @context << element
    when "Hsp_qseq"
      @context << element
    when "Hsp_hseq"
      @context << element      
    when "Hsp_midline"
      @context << element
    end
  end
  
  def end_element(element)
    case element
    when "Iteration"
      @context.pop
    when "Iteration_message"
      if @iteration_message == "No hits found"
        @hsp = FakeHSP.new(@query_def)
        puts @hsp.to_prolog
      end
      @iteration_message == "reset"
      @context.pop
    when "Iteration_query-def"
      @context.pop
    when "Hsp"
      @hsp = HSP.new(@query_def)
      @hsp.features[:evalue] = @evalue
      @hsp.features[:qseq] = @qseq.to_prolog_array
      @hsp.features[:hseq] = @hseq.to_prolog_array
      @hsp.features[:midline] = @midline.to_prolog_array
      puts @hsp.to_prolog
      @context.pop
    when "Hsp_evalue" 
      @context.pop      
    when "Hsp_qseq"
      @context.pop
    when "Hsp_hseq"
      @context.pop      
    when "Hsp_midline"
      @context.pop
    end
  end
  
  def characters(text)
    return if @context.nil?
    case @context.last
    when "Iteration_message" 
        @iteration_message = text
    when "Iteration_query-def"
      @query_def = text
    when "Hsp_evalue"
      @evalue = text
    when "Hsp_qseq"
      @qseq = text
    when "Hsp_hseq"
      @hseq = text
    when "Hsp_midline"
      @midline = text
    end
  end
end

parser = XML::SAX::Parser.new(PostCallbacks.new)
parser.parse_file(ARGV[0])
#parser.parse_file('blast.xml')

