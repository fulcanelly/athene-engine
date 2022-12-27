require 'recursive-open-struct'
require 'awesome_print'
require 'yaml'
require 'colored'


module StateExtension
    
    #TODO make it better
    def _?
        unless defined? @@locale then 
            @@locale = LocaleHandler.new().tap do 
                _1.load
            end
        end

        @@locale.get_for(:eng)
    end

end

class LocaleHandler  

    attr_accessor :assets

    def file() 
        File.open('./locale/locale.yml')
    end

    def load 
        self.assets = RecursiveOpenStruct.new(
            YAML.load(file), recurse_over_arrays: true)
    end
    
    def get_for(locale)
        LocaleStruct.new([locale], :eng, self.assets)
    end

end


#TODO add not found handling 
#(may be fallback locales and generating not found fil)

#it's aimed to make easier work with locale
# Locale.starting.say_hello(name: "Mile")
class LocaleStruct
    #TODO use default
    attr_accessor :path, :default, :obj

    def initialize(path, default, obj)
        self.path = path
        self.default = default
        self.obj = obj
    end
    
    def get_by_path(name)
        [*path, name].reduce(obj) do |last, key| 
            last[key]
        end
    end

    def method_missing(name, *args, **wargs)
        puts name.to_s.red
        str_or_hash = get_by_path(name)

        if str_or_hash.is_a? String 
            LocaleFormater.new(str_or_hash, wargs).format()
        else
            LocaleStruct.new(
                [*self.path, name], 
                self.default,
                self.obj)
           # super
        end
    end

end

#replaces text with variables
#exmpale 
# LocaleFormater.new("hello #{name}", {name: 'Mike'}).format
# should return "hello Mike"
class LocaleFormater 

    attr_accessor :text, :variables 

    def initialize(text, variables)
        @text = text
        @variables = variables
    end

    def _var_name(name)
        "\#{#{name.to_s}}"
    end

    def format
        result = text 
        variables.each do |key, value|
            result = result.gsub(_var_name(key), value)
        end
        return result
    end

end
