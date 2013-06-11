CREATE  TABLE IF NOT EXISTS `poet_tree`.`metres` (
  `id` INT(11) NOT NULL AUTO_INCREMENT ,
  `description` VARCHAR(30) NOT NULL ,
  `syllables` INT(11) NULL DEFAULT NULL ,
  `pattern` VARCHAR(5) NULL DEFAULT NULL ,
  PRIMARY KEY (`id`) )
ENGINE = InnoDB
AUTO_INCREMENT = 29
DEFAULT CHARACTER SET = latin1

CREATE  TABLE IF NOT EXISTS `poet_tree`.`authors` (
  `id` INT(11) NOT NULL AUTO_INCREMENT ,
  `name` VARCHAR(200) NOT NULL ,
  `birth_year` INT(4) NULL DEFAULT NULL ,
  `death_year` INT(4) NULL DEFAULT NULL ,
  PRIMARY KEY (`id`) )
ENGINE = InnoDB
AUTO_INCREMENT = 641
DEFAULT CHARACTER SET = latin1

CREATE  TABLE IF NOT EXISTS `poet_tree`.`dictionary` (
  `word` VARCHAR(150) NOT NULL ,
  `part_of_speech_id` INT(11) NULL DEFAULT NULL ,
  `metre_id` INT(11) NULL DEFAULT NULL ,
  PRIMARY KEY (`word`) ,
  INDEX `metre_id` (`metre_id` ASC) ,
  INDEX `part_of_speech_id` (`part_of_speech_id` ASC) ,
  CONSTRAINT `dictionary_ibfk_1`
    FOREIGN KEY (`metre_id` )
    REFERENCES `poet_tree`.`metres` (`id` ),
  CONSTRAINT `dictionary_ibfk_2`
    FOREIGN KEY (`part_of_speech_id` )
    REFERENCES `poet_tree`.`parts_of_speech` (`id` ))
ENGINE = InnoDB
DEFAULT CHARACTER SET = latin1

CREATE  TABLE IF NOT EXISTS `poet_tree`.`parts_of_speech` (
  `id` INT(11) NOT NULL AUTO_INCREMENT ,
  `description` VARCHAR(20) NOT NULL ,
  PRIMARY KEY (`id`) )
ENGINE = InnoDB
AUTO_INCREMENT = 9
DEFAULT CHARACTER SET = latin1



CREATE  TABLE IF NOT EXISTS `poet_tree`.`poetry` (
  `id` INT(11) NOT NULL AUTO_INCREMENT ,
  `author_id` INT(11) NOT NULL ,
  `title` VARCHAR(400) NOT NULL ,
  `poem_text` TEXT NOT NULL ,
  PRIMARY KEY (`id`) ,
  UNIQUE INDEX `author_id` (`author_id` ASC, `title` ASC) ,
  CONSTRAINT `poetry_ibfk_1`
    FOREIGN KEY (`author_id` )
    REFERENCES `poet_tree`.`authors` (`id` ))
ENGINE = InnoDB
DEFAULT CHARACTER SET = latin1


