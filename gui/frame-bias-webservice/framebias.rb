require 'rubygems'
require 'sinatra'

# 1) accept upload of GFF3 file or similar
# 2.1) accept upload of Golden standard file or 
# 2.2) selection of a parameter file for a particular organism + Manual
# delete probability
# 3) Enter position of origin and terminus 
# 4) report findings e.g. accuracy etc. 
#

get '/' do
        haml :index
end

