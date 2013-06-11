require 'rubygems'
require 'nokogiri'   
require 'open-uri'
require 'mysql'

begin
  	db = Mysql.new('localhost', '', '', 'poet_tree')
rescue Mysql::Error
  puts "Unable to connect to the database"
  exit 1
end

folder = '../rhythmicfeet'
	Dir.entries(folder).each{ |file|
		fileOpen = File.open(folder+"/"+file, "r")
		arr = File.basename(fileOpen, '.txt').split
		if(arr.length > 1)
			puts "arr is #{arr}"
			arr[1].gsub!('0', 'x')
			arr[1].gsub!('1', '/')
			pattern = arr[1].strip
			puts "#{pattern}"
			 fileOpen.each{ |line|
				 st1 = db.prepare("SELECT id FROM metres WHERE pattern = ?")
 					st1.execute pattern
  					r1 = st1.fetch	
  					puts "Word: #{line.strip.downcase!}. : #{r1[0]}, #{arr[1].length}, "
  					word = line.strip.downcase!
  					if(r1 && word != nil)
  					begin
					 insert_r = db.prepare "INSERT INTO dictionary (word, metre_id) VALUES (? ,?)"
					 insert_r.execute word, r1[0] 
					 insert_r.close
					 	rescue Mysql::Error
			puts 'error when adding poem ' + $!.message
		end
					 end
				 		 st1.close
			 }
		 end
	}
