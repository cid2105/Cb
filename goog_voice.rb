#!/Users/colediamond/.rvm/rubies/ruby-1.9.3-p194/bin/ruby
dict = {'alexanderingram' => "9178378859", 'ericmogil' => "5163758097", 'me' => "5169967037" }

num = ARGV.shift
message = ARGV.join(" ").downcase
num =  dict[num.downcase] unless num.match(/\d+/) 

system( "~/Public/goog_voice.pl -c sms -p #{num} #{message}" )