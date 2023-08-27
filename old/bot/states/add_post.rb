#TODO post settings (markdown, link preview)
#TODO add ability to add links buttons
#TODO add post preview
#TODO add ability to list/remove/edit them

class AddAdvertsPostState < BaseState

    def restart
        switch_state self.class.new

    end

    def more_or_exit
        suggest_it("Add more or continue ?")
            .option("Continue") do
                switch_state FindPartnersState.new
            end
            .option("Add more") do
                restart()
            end
            .exec
    end

    def make_text_post
        text = text_or_nil("Etner message text")
        restart() unless text
        say "Post added"
        more_or_exit()
    end

    def run

        suggest_it("Select type of post:")
            .option('Text') do
                make_text_post()
            end
            .option('GIF') do in_dev() end
            .option('Photo') do in_dev() end
            .option('Video') do in_dev() end
            .option('Back') do
                switch_state MainMenuState.new
            end
            .exec
    end

    
    def in_dev
        say "In developemnt"
        restart()
    end


end
