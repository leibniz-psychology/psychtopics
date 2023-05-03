
$(document).ready(function() {
  
  // activate the slider on start
  Shiny.addCustomMessageHandler('setSlider', function(arg) {
    Shiny.setInputValue(arg.id, arg.vals);
  })
  
  // click go once a year range has been selected with the slider
  Shiny.addCustomMessageHandler('clickGo', function(arg) {
    $("#" + arg.button).click();
  })
  
  // pick a topic from the people picker on start
  Shiny.addCustomMessageHandler('pickOne', function(arg) {
    $(".ms-BasePicker-input").click();
    $("#sug-0").click();
  })
  
  // handler for tag picker
  Shiny.addCustomMessageHandler(
    type = 'updateTopicIds', function(arg) {
      window.topicIds = arg.values.map((topic, index) => ({ key: (index + 1), name: topic }) );
      Shiny.setInputValue('topic_evol-search', topicIds.slice(0, 1));
  });
  
  
  // set a variable with all the topic evo terms
  Shiny.addCustomMessageHandler('setTopicEvoTerms', function(arg) {
    window.topicEvoTerms = arg.terms.join("; ");
  });
  
  Shiny.addCustomMessageHandler('initiateWordEmbeddings', function(arg) {
    // Initialize typeahead for search input in table
    $('#browse-topics_table .rt-search').typeahead({
      hint: true,
      highlight: false,
      minLength: 1
    },
    // Specify source of suggestions                                          
    {
      name: 'wordVecs',
      source: substringMatcher(wordVecs)
    });
    
        
        
    // select and enter key press reactions in search Browse Topics; handle click on typeahead suggestion
    $('.tt-menu').on('click', function() {
      // Get input value from typeahead and update table search
      x = $(".tt-input").val();
      Reactable.setSearch("browse-topics_table", x);
    });
    
    
    // Handle enter key press in typeahead input
    $('.tt-input').keydown(function(e){
      if (e.which == 13) { 
        // Get input value from typeahead and update table search
        x = $(".tt-input").val();
        Reactable.setSearch("browse-topics_table", x);
        return false;
      }
    });
   
   // Add a header to the typeahead dropdown
   $('.tt-menu').prepend('<div class="tt-header">Similar words in Psychtopics</div>');
    
  });
  
  






  
  //$("#start-dropdown_most_popular2").hide();
  // This code block registers icons for Fluent UI React. It maps each icon name to a corresponding icon component or Unicode character.
  jsmodule['@fluentui/react'].registerIcons({
    icons: {
      //'Cap': <Icon icon={'link'} />,
      Cap: React.createElement('Icon', {icon: "link"}),
      'Ups': '\uE417'
      //Filters: <FontAwesomeIcon icon={faFilter} />
      //'HomeSolid': <Icon icon={['fas', 'home']} />,
    }
  });
  
  //console.log(React.createElement('Icon', {icon: 'fa-link'}));
  
  
  
  // menu for small screens
  // This code block handles the click event of the menu button and the links in a navigation bar. 
  // If the window width is less than 768 pixels, it toggles the sidenav-opened, main-opened, and title2-opened classes on certain elements, to show or hide a side navigation menu.
  $("#menu, .ms-Nav-compositeLink a").click(function(e) {
    //console.log("clicked");
    
    if( $(window).width() < 768 ){
      // do your stuff
      $(".sidenav").toggleClass("sidenav-opened");
      $(".main").toggleClass("main-opened");
      $(".title2").toggleClass("title2-opened");
    }
    

    //$('.grid-container').css({ "grid-template-columns" : "50% 50%", "transition": "all 1s" });
    //$('.sidenav').css({ "width": "100%", "transition": "all 1s" })
  });
  
  //var chart = $('#htmlwidget_container').highcharts();
  //chart.series[0].data[1].select(true, true);
  
  // hide menu on click outside
  // This code block handles the click event on the document object. 
  // If the clicked element is not the side navigation menu, the menu button, or any of their parents, and the window width is less than 768 pixels, it removes the sidenav-opened, main-opened, and title2-opened classes on certain elements, to hide the side navigation menu.
  $('html').click(function(e) {
    //if clicked element is not your element and parents aren't your div
    if (e.target.id != '.sidenav' && e.target.id != '.menu' && $(e.target).parents('.sidenav').length == 0 && $(e.target).parents('.menu').length == 0 && $(window).width() < 768 && $('.sidenav').hasClass("sidenav-opened")) {
      $(".sidenav").removeClass("sidenav-opened");
      $(".main").removeClass("main-opened");
      $(".title2").removeClass("title2-opened");
    }
  });
  
});


// This code defines two pure JS functions for a tag picker:
// pure js

// functions for tag picker:
// This function takes in a tag and a tagList array as arguments. 
// It checks if tagList is empty, in which case it returns false. If tagList is not empty, it uses the some() method to check if any element in tagList matches tag by key. 
// If there is a match, it returns true; otherwise, it returns false.
function listContainsTagList(tag, tagList) {
  if (!tagList || !tagList.length || tagList.length === 0) {
    return false;
  }
  return tagList.some(compareTag => compareTag.key === tag.key);
};

// This function takes in a filterText string and a tagList array as arguments. 
// It checks if filterText is truthy, in which case it filters the tagList array to include only tags whose name property includes filterText (case-insensitive) and that are not already in tagList by key. 
// If filterText is falsy, it returns an empty array. The filtered array is then returned.
function filterSuggestedTags(filterText, tagList) {
  return filterText
    ? topicIds.filter(
        tag => tag.name.toLowerCase().includes(filterText.toLowerCase()) && !listContainsTagList(tag, tagList),
      )
    : [];
};


// word-embeddings in search in Browse topics


var substringMatcher = function(strs) {
  return function findMatches(q, cb) {
    var matches, substringRegex;
    
    // an array that will be populated with substring matches
    q = q.length > 0 ? q.toLowerCase() : q;
    matches = [];
    
    // regex used to determine if a string contains the substring `q`
    //substrRegex = new RegExp(q, 'i');
    
    // iterate through the pool of strings and for any string that
    // contains the substring `q`, add it to the `matches` array

    
    matched = findSimilarWords(strs, 15, q).map(function(x) { return x[0] });;
    
    console.log("top 15 word-embeddings:", matched)
    
    $.each(matched, function(i, str) {
      substrRegex = new RegExp("\\b" + str + "\\b");
      if (substrRegex.test(topicEvoTerms) && str.length > 1) {
        matches.push(str);
      }
    });
    
    //matches = matched.filter(word => topicEvoTerms.includes(word))
    matches5 = matches.length >= 5 ? matches.slice(0, 5) : matches
    console.log("top matches with psychtopics words:", matches);
    cb(matches5);
  };
};




