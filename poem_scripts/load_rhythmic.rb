require 'rubygems'
require 'nokogiri'   
require 'open-uri'
require 'mysql'

begin
  	db = Mysql.new('localhost', 'root', '', 'poet_tree')
rescue Mysql::Error
    puts "Unable to connect to the database"
  exit 1
end

	Dir["../rhythmicfeet/*"].each{ |file|
		
		arr = file.basename(file, '.txt').split(' ')
		
		arr[1].sub(/[0]/, 'x')
		arr[1].sub(/[1]/, '/')
		 file.each do |line|
		 		puts "Word: #{line.strip} : #{arr[1]}"
		 end

	
	}
